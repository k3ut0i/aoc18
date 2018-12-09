#include <stdio.h>
#include <math.h>
#include <stdlib.h>

struct marble{
  int i;
  struct marble * next;
  struct marble * previous;
};
struct marble* new_marble(int n);

struct board{
  struct marble * current;
};

struct game{
  struct board* b;
  int num_players;
  int* score;
  int current_player;
  int current_marble;
};
void print_game(struct game*);
void step_game(struct game*);
void free_game(struct game*);

int main(int argc, char** argv){
  if(argc != 3)
    return -1;
  const int n = atoi(argv[1]);
  int max = atoi(argv[2]);

  /* Initialization */
  struct game* g = malloc(sizeof (struct game));
  g->b = malloc(sizeof(struct board));
  g->b->current = new_marble(0);
  g->num_players = n;
  g->score = malloc(n* sizeof(int));
  g->current_marble = 1;
  g->current_player = 0;

  print_game(g);

  while(g->current_marble <= max)
    step_game(g);

  print_game(g);

  /*  fprintf(stdout, "%d %d\n", n, max); */
}

struct marble* new_marble(int n)
{
  struct marble* m = malloc(sizeof(struct marble));
  m->i = n;
  m->next = m;
  m->previous = m;
  return m;
}

void print_game(struct game* g)
{
  fprintf(stdout, "Game Players: %d Current Player: %d Current Marble: %d\n",
	  g->num_players, g->current_player, g->current_marble);
  struct marble* marble_iter = g->b->current;

  for(int i = 0; i < g->current_marble; i++, marble_iter = marble_iter->next) {
    fprintf(stdout, "%d ", marble_iter->i);
    }
  fputc('\n', stdout);
  for(int i = 0; i < g->num_players; i++){
    fprintf(stdout, "%d ", g->score[i]);
  }
  fputc('\n', stdout);
}

void step_game(struct game* g)
{
  struct marble* new = new_marble(g->current_marble);
  if(g->current_marble % 23 == 0){
    struct marble* seventh = g->b->current;
    for(int i = 0; i < 7; i++){
      seventh = seventh->previous;
    }
    struct marble* left = seventh->previous;
    struct marble* right = seventh->next;
    left->next = right;
    right->previous = left;
    g->score[g->current_player] = g->current_marble + seventh->i;
    g->b->current = right;
  }else{
    struct marble* left = g->b->current->next;
    struct marble* right = g->b->current->next->next;
    left->next = new;
    new->previous = left;
    right->previous = new;
    new->next = right;
    g->b->current = new;
  }
  g->current_marble++;
  g->current_player = (g->current_player + 1) % g->num_players;
}

void free_game(struct game* g)
{
  struct marble* m = g->b->current;
  struct marble* next;
  while(m != NULL){
    next = m->next;
    free(m);
    m = next;
  }
  free(g->b);
  free(g);
}
/* Local Variables: */
/* compile-command: "gcc -Wall -Wextra day09_02.c -o day09_02 -lm" */
/* End: */
