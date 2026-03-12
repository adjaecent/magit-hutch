#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int x;
    int y;
} Point;

int add(int a, int b) {
    return a + b;
}

void print_point(Point *p) {
    printf("(%d, %d)\n", p->x, p->y);
}

int main(int argc, char *argv[]) {
    Point p = {1, 2};
    int sum = add(p.x, p.y);
    print_point(&p);
    return 0;
}
