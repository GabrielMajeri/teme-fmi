#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>

/// Integer matrix structure, storing data in a row-major format
typedef struct {
    int rows, columns;
    int** data;
} Matrix;

/// Allocate a new N by M matrix
Matrix alloc_matrix(int rows, int columns) {
    Matrix m = {
        .rows = rows,
        .columns = columns,
        .data = malloc(sizeof(int*) * rows),
    };

    for (int i = 0; i < rows; ++i) {
        m.data[i] = malloc(sizeof(int) * columns);
    }

    return m;
}

/// Reads and allocates a matrix of numbers from a file
Matrix read_matrix(FILE* fin) {
    Matrix m = {};

    fscanf(fin, "%d %d", &m.rows, &m.columns);

    m.data = malloc(sizeof(int*) * m.rows);

    for (int i = 0; i < m.rows; ++i) {
        m.data[i] = malloc(sizeof(int) * m.columns);
        for (int j = 0; j < m.columns; ++j) {
            fscanf(fin, "%d", &m.data[i][j]);
        }
    }

    return m;
}

/// Releases the memory used by a matrix
void free_matrix(Matrix* m) {
    for (int i = 0; i < m->rows; ++i) {
        free(m->data[i]);
    }
    free(m->data);
    m->data = NULL;
}

/// Structure containing input data for a worker thread
typedef struct {
    Matrix* ms;
    int row, column;
} MatMulInput;

/// Worker thread routine which multiplies a row with a column
void* multiply_row_col(void* input) {
    MatMulInput* mm = input;
    int n = mm->ms[0].columns;
    int** a = mm->ms[0].data;
    int** b = mm->ms[1].data;
    int row = mm->row;
    int column = mm->column;

    int* result = calloc(sizeof(int), 1);

    for (int k = 0; k < n; ++k) {
        *result += a[row][k] * b[k][column];
    }

    return result;
}

int main() {
	// Read the input
    FILE* fin = fopen("matmul.txt", "rt");
    if (!fin) {
        perror("Failed to open input file");
        return 1;
    }

    Matrix ms[] = {
        read_matrix(fin),
        read_matrix(fin),
    };

    fclose(fin);

	// Allocate space for the output
    Matrix result = alloc_matrix(ms[0].rows, ms[1].columns);

    int num_threads = result.rows * result.columns;
    MatMulInput* inputs = malloc(sizeof(MatMulInput) * num_threads);
    pthread_t* worker_ids = malloc(sizeof(pthread_t) * num_threads);

	// Launch the worker threads
    for (int i = 0; i < result.rows; ++i) {
        for (int j = 0; j < result.columns; ++j) {
            int index = i * result.columns + j;
            inputs[index] = (MatMulInput){ms, i, j};
            pthread_create(&worker_ids[index], NULL, multiply_row_col, &inputs[index]);
        }
    }

	// Collect the dot products from the workers
    for (int i = 0; i < result.rows; ++i) {
        for (int j = 0; j < result.columns; ++j) {
            void* result_ptr;
            pthread_join(worker_ids[i * result.columns + j], &result_ptr);
            result.data[i][j] = *(int*)result_ptr;
            free(result_ptr);
        }
    }

    printf("Result is %dx%d:\n", result.rows, result.columns);
    for (int i = 0; i < result.rows; ++i) {
        for (int j = 0; j < result.columns; ++j) {
            printf("%d ", result.data[i][j]);
        }
        printf("\n");
    }

    free(inputs);
    free(worker_ids);
    free_matrix(&ms[0]);
    free_matrix(&ms[1]);
    free_matrix(&result);
}
