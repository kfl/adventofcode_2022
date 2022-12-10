#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum { noop, addx} command_t;

typedef struct {
  command_t type;
  int arg;
} command;


command parse_line(char *line) {
  // We know that line is not need after parsing, thus it is OK to
  // modify it with strsep

  char *op = strsep(&line, " ");

  if (strncmp(op, "noop", 4) == 0) {
    return (command){.type = noop, .arg = 0};
  }

  // if not noop we assume addx and read the argument
  char *n_str = strsep(&line, " ");
  int n = strtol(n_str, NULL, 10);
  return (command){.type = addx, .arg = n};
}

typedef struct {
  int next;
  int interesting;
} state;

static state cycle_check(int x, int cycle, state s) {

  // For part 1 we maintain some state that we update
  if (cycle == s.next) {
    s.interesting += x * cycle;
    s.next += 40;
  }

  // For part 2 we just print the the result as we go
  int pixel_pos = (cycle - 1) % 40;
  printf("%c%s", abs(x - pixel_pos) <= 1 ? '#' : ' ',
                 pixel_pos == 39 ? "\n" : "");

  return s;
}

int interpret_commands(const char *filename) {
  FILE *input = fopen(filename, "r");
  if (!input) {
    printf("Cannot open %s\n", filename);
    exit(EXIT_FAILURE);
  }

  char *line = NULL;
  size_t line_len = 0;

  int x = 1;
  int cycle = 1;
  state s = {.next = 20,
             .interesting = 0};

  // Use `getline` to read the file line by line.
  while (getline(&line, &line_len, input) != -1) {
    command cmd = parse_line(line);

    s = cycle_check(x, cycle, s);

    switch (cmd.type) {
    case noop:
      cycle++;
      break;

    case addx:
      cycle++;
      s = cycle_check(x, cycle, s);
      x += cmd.arg;
      cycle ++;
      break;
    }
  }
  s = cycle_check(x, cycle, s);

  // printf("%d cycles completed, x ended as %d\n", cycle, x);

  free(line);
  fclose(input);

  return s.interesting;
}


int main(int argc, char *argv[]) {
  if (argc < 2) {
    printf("Usage: %s file\n", argv[0]);
    return EXIT_FAILURE;
  }
  const char *filename = argv[1];

  printf("Part 1: %d\n", interpret_commands(filename));

  return EXIT_SUCCESS;
}
