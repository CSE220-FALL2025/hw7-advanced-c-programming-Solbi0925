//Name : Solbi Park
//ID: 116739094
#include "hw7.h"

bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root) {
    if(root == NULL) {
        bst_sf *node = malloc(sizeof(bst_sf));
        node->mat = mat;
        node->left_child = NULL;
        node->right_child = NULL;
        return node;
    }
    else{
        if((mat->name)<(root ->mat->name)){
            root ->left_child = insert_bst_sf(mat,root->left_child);
        } else{
            root -> right_child= insert_bst_sf(mat,root->right_child);
        }
        return root;
    }
}

matrix_sf* find_bst_sf(char name, bst_sf *root) {
    if(root == NULL) return NULL;
    if((root->mat->name)==name) return root->mat;
    else{
        if(name<(root->mat->name)) return find_bst_sf(name, root->left_child);
        else{ return find_bst_sf(name, root->right_child); }
    }
}

void free_bst_sf(bst_sf *root) {
    if(root==NULL) return;
    else{
        free_bst_sf(root->left_child);
        free_bst_sf(root->right_child);
        free(root->mat);
        free(root);
    }
}

matrix_sf* add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    matrix_sf *mat_sum = malloc(sizeof(matrix_sf)+(mat1->num_rows)*(mat1->num_cols)*sizeof(int));
    if (mat_sum == NULL) return NULL;
    mat_sum -> name = '!';
    mat_sum -> num_rows = mat1->num_rows;
    mat_sum -> num_cols = mat1 -> num_cols;

    int *temp1 = mat1 -> values;
    int *temp2 = mat2 -> values;
    int *temp3 = mat_sum -> values;

    int count = mat1->num_rows * mat1 -> num_cols;
    int i;
    for (i = 0; i < count; i++) {
        *temp3 = *temp1 + *temp2;
        temp1++;
        temp2++;
        temp3++;
    }
    return mat_sum;
}

matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    int m1r = mat1 -> num_rows;
    int m1c = mat1 -> num_cols;
    int m2r = mat2 -> num_rows;
    int m2c = mat2 -> num_cols;

    matrix_sf *mat_mul = malloc(sizeof(matrix_sf)+m1r*m2c*sizeof(int));
    if (mat_mul == NULL) return NULL;

    mat_mul -> name = '!';
    mat_mul -> num_rows = m1r;
    mat_mul -> num_cols = m2c;
    
    int *temp1 = mat1 -> values;
    int *temp2 = mat2 -> values;
    int *temp3 = mat_mul -> values;

    int i, j, k;
    for (i = 0; i < m1r; i++) {
        for (j = 0; j < m2c; j++) {
            int sum = 0;
            for (k = 0; k < m1c; k++) {
                int a = mat1->values[i*m1c + k];
                int b = mat2->values[k*m2c + j];
                sum += a * b;
            }
            mat_mul->values[i*m2c + j] = sum;
        }
    }

    return mat_mul;
}

matrix_sf* transpose_mat_sf(const matrix_sf *mat) {
    matrix_sf *mat_trans = malloc(sizeof(matrix_sf) + (mat->num_rows)*(mat->num_cols)*sizeof(int));
    if (mat_trans == NULL) return NULL;
    mat_trans -> name = '!';
    mat_trans -> num_rows = mat -> num_cols;
    mat_trans -> num_cols = mat -> num_rows;

    int i, j;
    for (i = 0; i < mat -> num_rows; i++) {
        for (j = 0; j < mat -> num_cols; j++) {
            mat_trans->values[j*(mat -> num_rows) + i] = mat->values[i*(mat -> num_cols) + j];
        }
    }
    return mat_trans;
}

matrix_sf* create_matrix_sf(char name, const char *expr) {
    const char *p = expr;

    while (*p != '\0' && isspace(*p)) {
        p++;
    }
    int num_rows = 0;
    while (*p >= '0' && *p <= '9') {
        num_rows = num_rows * 10 + (*p - '0');
        p++;
    }

    while (*p != '\0' && isspace(*p)) {
        p++;
    }

    int num_cols = 0;
    while (*p >= '0' && *p <= '9') {
        num_cols = num_cols * 10 + (*p - '0');
        p++;
    }

    while (*p != '\0' && *p != '[') {
        p++;
    }
    if (*p == '[') {
        p++;
    }

    int total = num_rows * num_cols;
    matrix_sf *m = malloc(sizeof(matrix_sf) + total * sizeof(int));
    if (m == NULL) return NULL;

    m->name = name;
    m->num_rows = num_rows;
    m->num_cols = num_cols;

    int idx = 0;

    while (*p != '\0' && idx < total) {
        while (*p != '\0' &&
               (isspace(*p) || *p == ';')) {
            p++;
        }

        if (*p == ']') {
            break;
        }

        int sign = 1;
        if (*p == '-') {
            sign = -1;
            p++;
        }

        int val = 0;
        while (*p >= '0' && *p <= '9') {
            val = val * 10 + (*p - '0');
            p++;
        }
            m->values[idx++] = sign * val;
    }
    return m;
}



int precedence(char op) {
    if(op == '\'') return 3;
    if(op == '*') return 2;
    if(op == '+') return 1;
    return 0;
}



typedef struct{
    char stack[100];
    int top;
} Stack;

void init_stack(Stack *s){
    s->top = -1;
}

int is_empty(Stack *s){
    return s->top == -1;
}


void push(Stack *s, char c){
    s-> stack[++(s->top)] =c;
}

char pop(Stack *s){
    return s->stack[(s->top)--];
}

char peek(Stack *s){
    return s->stack[s->top];
}



char* infix2postfix_sf(char *infix) {
    char *postfix = malloc(strlen(infix)+1);
    if(postfix==NULL) return NULL;

    char *out = postfix;
    Stack op_stack;
    init_stack(&op_stack);

    while (*infix) {
        char c = *infix;

        if (isspace(c)) {
            infix++;
            continue;
        }

        if (isalpha(c)) {
            *out++ = c;
            infix++;
            continue;
        }

        if (c == '(') {
            push(&op_stack, c);
            infix++;
            continue;
        }

        if (c == ')') {
            while (!is_empty(&op_stack) && peek(&op_stack) != '(') {
                *out++ = pop(&op_stack);
            }
            if (!is_empty(&op_stack) && peek(&op_stack) == '(') {
                pop(&op_stack);
            }
            infix++;
            continue;
        }

        if (c == '\'') {
            *out++ = c;
            infix++;
            continue;
        }

        if (c == '+' || c == '*') {
            while (!is_empty(&op_stack) &&
                   peek(&op_stack) != '(' &&
                   precedence(peek(&op_stack)) >= precedence(c)) {
                *out++ = pop(&op_stack);
            }
            push(&op_stack, c);
            infix++;
            continue;
        }
        infix++;
    }
    while (!is_empty(&op_stack)) {
        char op = pop(&op_stack);
        if (op != '(') { 
            *out++ = op;
        }
    }

    *out = '\0';
    return postfix;
}

typedef struct {
    matrix_sf *data[100];
    int top;
} MatStack;

void mstack_init(MatStack *s) {
    s->top = -1;
}

int mstack_is_empty(MatStack *s) {
    return s->top == -1;
}

void mstack_push(MatStack *s, matrix_sf *m) {
    s->data[++(s->top)] = m;
}

matrix_sf* mstack_pop(MatStack *s) {
    return s->data[(s->top)--];
}

matrix_sf* mstack_peek(MatStack *s) {
    return s->data[s->top];
}





matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root) {
    char *postfix = infix2postfix_sf(expr);
    if(postfix == NULL) return NULL;

    MatStack stack;
    mstack_init(&stack);

    char *p = postfix;
    while(*p){
        char c = *p;
        if(isspace(c)){
            p++;
            continue;
        }
        if(isalpha(c)){
            matrix_sf *m = find_bst_sf(c,root);
            mstack_push(&stack,m);
            p++;
            continue;
        }
        if(c=='\''){
            matrix_sf *m = mstack_pop(&stack);
            matrix_sf *t = transpose_mat_sf(m);

            if(m->name < 'A'||m->name >'Z') free(m);
            mstack_push(&stack,t);
            p++;
            continue;
        }

        if((c=='+')||(c=='*')){
            matrix_sf *right = mstack_pop(&stack);
            matrix_sf *left = mstack_pop(&stack);
            matrix_sf *res;

            if(c=='+'){
                res = add_mats_sf(left,right);
            } else{
                res = mult_mats_sf(left,right);
            }

            if (left->name < 'A'||left->name >'Z') {
                free(left);
            }
            if (right->name < 'A'||right->name >'Z') {
                free(right);
            }

            mstack_push(&stack, res);
            p++;
            continue;
        }
        p++;
    }
    matrix_sf *result = mstack_pop(&stack);
    result->name = name;
    free(postfix);
    return result;
}

matrix_sf *execute_script_sf(char *filename) {
   FILE *file = fopen(filename, "r");
    if (file == NULL) return NULL;

    bst_sf *root = NULL;
    matrix_sf *last_mat = NULL;

    char line[1000];  

    while (fgets(line, 1000, file) != NULL) {

        char *p = line;

        while (*p && isspace(*p)) p++;
        if (*p == '\0' || *p == '\n') continue;

        char name = *p;
        p++;

        while (*p && isspace(*p)) p++;
        if (*p != '=') continue;
        p++;

        while (*p && isspace(*p)) p++;
        if (*p == '\0' || *p == '\n') continue;

        if (isdigit(*p)) {
            matrix_sf *m = create_matrix_sf(name, p);
            root = insert_bst_sf(m, root);
            last_mat = m;
        }
        else {
            matrix_sf *m = evaluate_expr_sf(name, p, root);
            root = insert_bst_sf(m, root);
            last_mat = m;
        }
    }

    fclose(file);
    return last_mat;
}

// This is a utility function used during testing. Feel free to adapt the code to implement some of
// the assignment. Feel equally free to ignore it.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[]) {
    matrix_sf *m = malloc(sizeof(matrix_sf)+num_rows*num_cols*sizeof(int));
    m->name = '?';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows*num_cols*sizeof(int));
    return m;
}

// Don't touch this function. It's used by the testing framework.
// It's been left here in case it helps you debug and test your code.
void print_matrix_sf(matrix_sf *mat) {
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (unsigned int i = 0; i < mat->num_rows*mat->num_cols; i++) {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows*mat->num_cols-1)
            printf(" ");
    }
    printf("\n");
}
