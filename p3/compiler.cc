#include <iostream>
#include <cstdarg>
#include <unordered_map>

#include "lexer.h"
#include "execute.h"

using namespace std;

#define DEBUG 0     // 1 => Turn ON debugging, 0 => Turn OFF debugging

static LexicalAnalyzer lexer;
static int mem_available = 0;
unordered_map<string, int> var_map;
unordered_map<InstructionNode*, int> instmap;

extern string reserved[];

static void dprintf(const char* format, ...)
{
    va_list args;
    if (DEBUG)
    {
        va_start (args, format);
        vfprintf (stdout, format, args);
        va_end (args);
    }
}

// add ID or NUM token to mem
// return index in mem
int get_or_add_mem(const Token& tk)
{
    if (tk.token_type == NUM) {
        mem[mem_available] = stoi(tk.lexeme);
    } else { // type ID
        mem[mem_available] = 0;
        var_map[tk.lexeme] = mem_available;
    }
    return mem_available++;
}

bool parse_condition(InstructionNode& inst)
{
    // Helper to get operand index based on token type
    auto get_operand_index = [](const Token& token) {
        if (token.token_type == ID) {
            return var_map[token.lexeme];
        }
        else { // type NUM
            return get_or_add_mem(token);
        }
        };

    // Helper to map token to condition operator
    auto get_condition_operator = [](const Token& token) -> ConditionalOperatorType {
        switch (token.token_type) {
        case NOTEQUAL:
            return CONDITION_NOTEQUAL;
        case GREATER:
            return CONDITION_GREATER;
        case LESS:
            return CONDITION_LESS;
        default:
            dprintf("ERROR: unexpected condition op %s\n", token.lexeme.c_str());
            throw std::runtime_error("Invalid condition operator");
        }
        };

    try {
        // Parse tokens
        Token op1 = lexer.GetToken();
        Token op = lexer.GetToken();
        Token op2 = lexer.GetToken();

        // Initialize instruction type
        inst.type = CJMP;

        // Set operand indices
        inst.cjmp_inst.opernd1_index = get_operand_index(op1);
        inst.cjmp_inst.opernd2_index = get_operand_index(op2);

        // Set condition operator
        inst.cjmp_inst.condition_op = get_condition_operator(op);

        return true;
    }
    catch (const std::runtime_error&) {
        return false; // Return false if an invalid operator is encountered
    }
}

// output into inst
bool parse_assign(InstructionNode& inst, Token id)
{
    Token tmptk;
    Token op1;
    Token op;
    Token op2;

    inst.type = ASSIGN;
    inst.assign_inst.left_hand_side_index = var_map[id.lexeme];

    tmptk = lexer.GetToken(); // should be =
    op1 = lexer.GetToken();
    if (op1.token_type == ID) {
        inst.assign_inst.opernd1_index = var_map[op1.lexeme];
    } else { // type NUM
        inst.assign_inst.opernd1_index = get_or_add_mem(op1);
    }
    op = lexer.GetToken();

    if (op.token_type == SEMICOLON) {
        inst.assign_inst.op = OPERATOR_NONE;
    } else {
        switch (op.token_type) {
            case PLUS:
                inst.assign_inst.op = OPERATOR_PLUS;
                break;
            case MINUS:
                inst.assign_inst.op = OPERATOR_MINUS;
                break;
            case DIV:
                inst.assign_inst.op = OPERATOR_DIV;
                break;
            case MULT:
                inst.assign_inst.op = OPERATOR_MULT;
                break;
            default:
                break;
        }
        op2 = lexer.GetToken();
        switch (op2.token_type) {
            case ID:
                inst.assign_inst.opernd2_index = var_map[op2.lexeme];
                break;
            case NUM:
                inst.assign_inst.opernd2_index = get_or_add_mem(op2);
                break;
            default:
                break;
        }
        lexer.GetToken(); // consume SEMICOLON
    }
    
    dprintf("assign: %s = %s[%d] %s\n", id.lexeme.c_str(), op1.lexeme.c_str(), inst.assign_inst.opernd1_index, op.lexeme.c_str());
    inst.next = nullptr;
    return true;
}

bool parse_body(InstructionNode& head);

// Helper function for ID case (assign)
void handle_assign_case(InstructionNode*& header, const Token& tk) {
    InstructionNode* inst = new InstructionNode;
    parse_assign(*inst, tk);
    inst->next = nullptr;
    header->next = inst;
    header = header->next;
}

// Helper function for INPUT case
void handle_input_case(InstructionNode*& header) {
    Token tk = lexer.GetToken();
    InstructionNode* inst = new InstructionNode;
    inst->type = IN;
    inst->input_inst.var_index = var_map[tk.lexeme];
    inst->next = nullptr;
    header->next = inst;
    header = header->next;
    lexer.GetToken(); // Consume SEMICOLON
}

// Helper function for OUTPUT case
void handle_output_case(InstructionNode*& header) {
    Token tk = lexer.GetToken();
    InstructionNode* inst = new InstructionNode;
    inst->type = OUT;
    inst->output_inst.var_index = var_map[tk.lexeme];
    inst->next = nullptr;
    header->next = inst;
    header = header->next;
    lexer.GetToken(); // Consume SEMICOLON
}

// Helper function for IF case
void handle_if_case(InstructionNode*& header) {
    InstructionNode* inst = new InstructionNode;
    parse_condition(*inst);

    InstructionNode ifbody;
    parse_body(ifbody);

    inst->next = ifbody.next;
    header->next = inst;
    while (header->next != nullptr)
        header = header->next;

    InstructionNode* noop = new InstructionNode;
    noop->type = NOOP;
    noop->next = nullptr;
    inst->cjmp_inst.target = noop;
    header->next = noop;
    header = header->next;
}

// Helper function for WHILE case
void handle_while_case(InstructionNode*& header) {
    InstructionNode* inst = new InstructionNode;
    parse_condition(*inst);

    InstructionNode whilebody;
    parse_body(whilebody);

    InstructionNode* jmp = new InstructionNode;
    jmp->type = JMP;
    jmp->jmp_inst.target = inst;

    InstructionNode* noop = new InstructionNode;
    noop->type = NOOP;
    noop->next = nullptr;

    inst->next = whilebody.next;
    header->next = inst;
    while (header->next != nullptr)
        header = header->next;

    inst->cjmp_inst.target = noop;
    header->next = jmp;
    jmp->next = noop;
    header = noop;
}

// Helper function for FOR case
void handle_for_case(InstructionNode*& header) {
    InstructionNode* assign1 = new InstructionNode;
    lexer.GetToken(); // LPAREN

    // Parse initial assignment
    Token tk = lexer.GetToken();
    parse_assign(*assign1, tk);
    assign1->next = nullptr;
    header->next = assign1;
    header = assign1;

    // Parse condition
    InstructionNode* cond = new InstructionNode;
    parse_condition(*cond);
    lexer.GetToken(); // SEMICOLON

    // Parse loop assignment
    InstructionNode* assign2 = new InstructionNode;
    tk = lexer.GetToken();
    parse_assign(*assign2, tk);
    lexer.GetToken(); // RPAREN

    // Parse body
    InstructionNode forbody;
    parse_body(forbody);

    // Add JMP back and NOOP
    InstructionNode* jmp = new InstructionNode;
    jmp->type = JMP;
    InstructionNode* noop = new InstructionNode;
    noop->type = NOOP;

    // Assemble pieces
    header->next = cond;
    cond->next = forbody.next;
    cond->cjmp_inst.target = noop;
    while (header->next != nullptr)
        header = header->next;

    header->next = assign2;
    assign2->next = jmp;
    jmp->jmp_inst.target = cond;
    jmp->next = noop;
    noop->next = nullptr;

    header = noop;
}

void handle_switch_case(InstructionNode*& header) {
    Token swtid = lexer.GetToken(); // Get the switch variable ID
    dprintf("SWITCH: variable = %s\n", swtid.lexeme.c_str());
    lexer.GetToken(); // Consume LBRACE

    // Create a NOOP instruction for the end of the switch
    InstructionNode* swtnoop = new InstructionNode;
    swtnoop->type = NOOP;
    swtnoop->next = nullptr;
    dprintf("SWITCH: Created NOOP instruction for end of switch\n");

    Token tk = lexer.GetToken(); // Start processing cases or default
    while (tk.token_type != RBRACE) {
        switch (tk.token_type) {
        case CASE: {
            Token caseValue = lexer.GetToken(); // Get case value (NUM)
            dprintf("SWITCH CASE: case value = %s\n", caseValue.lexeme.c_str());
            lexer.GetToken(); // Consume COLON

            // Parse the body of the case
            InstructionNode casebody;
            parse_body(casebody);
            dprintf("SWITCH CASE: Parsed body for case value = %s\n", caseValue.lexeme.c_str());

            // Create NOOP and JMP for case handling
            InstructionNode* noop = new InstructionNode;
            noop->type = NOOP;
            noop->next = nullptr;
            dprintf("SWITCH CASE: Created NOOP for case value = %s\n", caseValue.lexeme.c_str());

            InstructionNode* jmp = new InstructionNode;
            jmp->type = JMP;
            jmp->jmp_inst.target = swtnoop;
            dprintf("SWITCH CASE: Created JMP to end of switch for case value = %s\n", caseValue.lexeme.c_str());

            // Create a conditional jump for the case
            InstructionNode* cond = new InstructionNode;
            cond->type = CJMP;
            cond->next = noop;
            cond->cjmp_inst.opernd1_index = var_map[swtid.lexeme];
            cond->cjmp_inst.opernd2_index = get_or_add_mem(caseValue);
            cond->cjmp_inst.condition_op = CONDITION_NOTEQUAL;
            cond->cjmp_inst.target = casebody.next;
            dprintf("SWITCH CASE: Created CJMP for case value = %s\n", caseValue.lexeme.c_str());

            // Link the conditional jump to the header
            header->next = cond;

            // Traverse to the end of the case body
            header = casebody.next;
            while (header->next != nullptr)
                header = header->next;

            // Link the jump to NOOP
            header->next = jmp;
            jmp->next = noop;
            header = noop;
            break;
        }
        case DEFAULT: {
            lexer.GetToken(); // Consume COLON
            dprintf("SWITCH DEFAULT: Parsing default case body\n");

            // Parse the body of the default case
            InstructionNode dftbody;
            parse_body(dftbody);
            dprintf("SWITCH DEFAULT: Parsed body for default case\n");

            // Link the default body to the header
            header->next = dftbody.next;
            while (header->next != nullptr)
                header = header->next;
            break;
        }
        default:
            dprintf("ERROR: Unexpected token in switch statement: %s\n", tk.lexeme.c_str());
            break;
        }
        tk = lexer.GetToken(); // Fetch the next token for the loop
    }

    // Link the end of the switch to the NOOP instruction
    header->next = swtnoop;
    header = swtnoop;
    dprintf("SWITCH: Linked all cases and default to the final NOOP instruction\n");
}

// Main parse_body function
bool parse_body(InstructionNode& head) {
    InstructionNode* header = &head;
    Token tk = lexer.GetToken();

    if (tk.token_type != LBRACE) {
        return false;
    }

    tk = lexer.GetToken();
    while (tk.token_type != RBRACE) {
        // The main switch case structure
        switch (tk.token_type) {
        case ID:
            handle_assign_case(header, tk); // Handles the ID (assign) case
            break;
        case INPUT:
            handle_input_case(header); // Handles the INPUT case
            break;
        case OUTPUT:
            handle_output_case(header); // Handles the OUTPUT case
            break;
        case IF:
            handle_if_case(header); // Handles the IF case
            break;
        case WHILE:
            handle_while_case(header); // Handles the WHILE case
            break;
        case FOR:
            handle_for_case(header); // Handles the FOR case
            break;
        case SWITCH:
            handle_switch_case(header); // Handles the SWITCH case (to be defined)
            break;
        default:
            dprintf("ERROR: unexpected token %s type %d\n", tk.lexeme.c_str(), tk.token_type);
            break;
        }
        tk = lexer.GetToken(); // Fetch the next token
    }

    return true;
}


bool parse_var_section()
{
    Token tk = lexer.GetToken();
    while (tk.token_type != SEMICOLON) {
        if (tk.token_type == ID) {
            mem[mem_available] = 0;
            var_map[tk.lexeme] = mem_available;
            mem_available++;
        }
        tk = lexer.GetToken();
    }
    dprintf("[debug] var_list: ");
    for (auto it = var_map.begin(); it != var_map.end(); it++)
        dprintf("%s[%d],", it->first.c_str(), it->second);
    dprintf("\n");
    return true;
}

// parse inputs of program -> var_section body inputs
bool parse_inputs(InstructionNode& head)
{
    Token tk = lexer.GetToken();
    while (tk.token_type == NUM) {
        inputs.push_back(stoi(tk.lexeme));
        tk = lexer.GetToken();
    }
    dprintf("[debug] inputs list: ");
    for (auto it = inputs.begin(); it != inputs.end(); it++)
        dprintf("%d,", *it);
    dprintf("\n");
    return true;   
}

bool parse_program(InstructionNode& head)
{
    if (lexer.peek(1).token_type == ID) {
        parse_var_section();
    } else {
        return false;// there must be at least one var
    }
    parse_body(head);
    parse_inputs(head);
    return true;
}

struct InstructionNode* parse_Generate_Intermediate_Representation()
{
    InstructionNode head;

    parse_program(head);

    //dprintInsts(head);

    return head.next;
}