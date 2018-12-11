#include <stdio.h>
#include <math.h>
#include <stdlib.h>

/* Next and Previous are named w.r.t clockwise direction  */
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
  long long* score;
  int current_player;
  int current_marble;
};
struct game* new_game(int num_players);
void print_game(struct game*);
void print_board(struct game*);
void print_scores(struct game*);
void step_game(struct game*);
void free_game(struct game*);
long long max_score(struct game*);

int main(int argc, char** argv){
  if(argc != 3)
    return -1;
  const int n = atoi(argv[1]);
  int max = atoi(argv[2]);

  struct game* g = new_game(n);
  while(g->current_marble <= max){
    step_game(g);
    /*    print_board(g);*/
  }
  fprintf(stdout, "Maxscore: %lld\n", max_score(g));
  return 0;
}

struct marble* new_marble(int n)
{
  struct marble* m = malloc(sizeof(struct marble));
  m->i = n;
  m->next = m;
  m->previous = m;
  return m;
}

struct game* new_game(int n)
{
  struct game* g = malloc(sizeof (struct game));
  g->b = malloc(sizeof(struct board));
  g->b->current = new_marble(0);
  g->num_players = n;
  g->score = malloc(n* sizeof(long long));
  g->current_marble = 1;
  g->current_player = 0;
  return g;
}

void print_board(struct game* g)
{
  struct marble* head = g->b->current;
  struct marble* marble_iter = head;
  do{
    fprintf(stdout, "%d ", marble_iter->i);
    marble_iter = marble_iter->next;
  }while(marble_iter != head);
  
  fputc('\n', stdout);
}

void print_game(struct game* g)
{
  fprintf(stdout, "Game Players: %d Current Player: %d Current Marble: %d\n",
	  g->num_players, g->current_player, g->current_marble);
  print_board(g);
  print_scores(g);
}

void print_scores(struct game* g){
  for(int i = 0; i < g->num_players; i++){
    fprintf(stdout, "%lld ", g->score[i]);
  }
  fputc('\n', stdout);  
}
long long max_score(struct game* g){
  long long max = 0;
  for(int i = 0; i < g->num_players; i++){
    if(g->score[i] > max) max = g->score[i];
  }
  return max;
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
    g->score[g->current_player] += g->current_marble + seventh->i;
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
  g->current_player = (g->current_player + 1) % g->num_players;
  g->current_marble++;
}

void free_game(struct game* g)
{
  struct marble* m = g->b->current;
  struct marble* next;
  while(m != NULL){
    next = m->next;
    free(m);
    next->previous = NULL;
    m = next;
  }
  free(g->b);
  free(g);
}
/* Local Variables: */
/* compile-command: "gcc -Wall -Wextra day09_02.c -o day09_02 -lm" */
/* End: */
