#include  <R.h>
#include  <Rinternals.h>
#include  <Rmath.h>
#include  <Rdefines.h>
#include  <strings.h>
#include  <assert.h>

typedef enum { false, true } bool;

const unsigned int Prime = 0x01000193; //   16777619
const unsigned int Seed  = 0x811C9DC5; // 2166136261
/// hash a single byte
inline unsigned int fnv1a_char(unsigned char oneByte, unsigned int hash){
  return (oneByte ^ hash) * Prime;
}

unsigned int fnv1a(const char* text, unsigned int hash)
{
  assert(text);
  while (*text)
    hash = fnv1a_char((unsigned char)*text++, hash);
  return hash;
}


typedef struct tnode *Tptr;
typedef struct tnode{
  unsigned int count, ngram;
  double score;
  
  Tptr lhs, rhs;
} Tnode;


typedef struct tokennode *Tokenptr;
typedef struct tokennode{
  Tokenptr next;
  char *token;
} TokenNode;

Tnode *treeAlloc(void);
void freeTree(Tnode *tree);
Tnode *addTree(Tnode *tree, int gram);
void printTree(Tnode *tree);
Tnode *createTree(Tnode *tree, const char* word, const int n, int *gram_count);
void fixScore(Tnode *tree, int tot_count);
double scoreLookup(Tnode *tree, int gram);
double scoreTree(Tnode *treeA, Tnode *treeB);


TokenNode *tokenAlloc(void){
  return (TokenNode *) malloc (sizeof(TokenNode));
}

void freeTokenList(TokenNode *tList){
  if (tList == NULL){
    return;
  }
  freeTokenList(tList->next);

  if(tList->token != NULL){
    free(tList->token);
    tList->token = NULL;
  }

  free(tList);
  
  return;
}

void printTokenList(TokenNode *tList){
  TokenNode *ptr = tList;
  
  while(ptr != NULL && ptr->token != NULL){
    ptr = ptr->next;
  }

  return;
}

TokenNode *createTokenList(TokenNode *tList, const char *sentence){
  char *conSentence;
  int strSize = (strlen(sentence)+1)*sizeof(char);
  conSentence = (char*) malloc (strSize);
  memcpy(conSentence, sentence, (strSize));
  TokenNode *tEnd = NULL;

  char *pch;
  pch = strtok(conSentence, " ");
  while (pch != NULL){
    if(tList == NULL){
      tList = tokenAlloc();
      tEnd = tList;
    } else {
      while(tEnd->next != NULL){
        tEnd = tEnd->next;
      }
      tEnd->next = tokenAlloc();
      tEnd = tEnd->next;
    }
    strSize = (strlen(pch)+1)*sizeof(char);
    tEnd->token = (char*) malloc (strSize);
    memcpy(tEnd->token, pch, (strSize));
    tEnd->next = NULL;

    pch = strtok(NULL, " ");
  }
  
//  printTokenList(tList);
  return(tList);
}

Tnode *createWordTree(Tnode *tree, TokenNode const* const wordList, const int n, const int sentenceSize, int *gram_count){
  char *buffStart = NULL;
  char *buffLoc = NULL;
  char *buff = NULL;
  
  buffStart = (char*) malloc (sentenceSize);
  int allocLength;
  TokenNode const* currentPlace = wordList;
  TokenNode const* incLocation;
  
  bool shouldExit = false;
  int gramCT = 0;
  do{
    allocLength = 0;
    incLocation = currentPlace;
    buffLoc = buffStart;
    
    for(int i = 0; i < n; i++){
      if(incLocation == NULL || incLocation->token == NULL){
        shouldExit = true;
        break;
      }
      int strSize = strlen(incLocation->token);
      strncpy(buffLoc, incLocation->token, strSize);
      allocLength += strSize;
      buffLoc += strSize;
      
      incLocation = incLocation->next;
    }
    if(!shouldExit){
      buff = (char*) malloc (allocLength + 1);
      buff[allocLength] = '\0';
      strncpy(buff, buffStart, allocLength);
 //     printf("::%s::%i::%i::\n", buff, fnv1a(buff, Seed),*gram_count);
      tree = addTree(tree, fnv1a(buff, Seed));
      free(buff);
      gramCT++;

      currentPlace = currentPlace->next;
    }
  } while (incLocation != NULL && currentPlace != NULL && !shouldExit);

  fixScore(tree, gramCT);

  *gram_count = gramCT;
//  printf("%i", gramCT);
  free(buffStart);
  
//  printTree(tree);
  return tree;
}


SEXP NGramLetters(SEXP strA, SEXP strB, SEXP N){
  const char *myStrA = CHAR(STRING_ELT(strA,0));
  const char *myStrB = CHAR(STRING_ELT(strB,0));
  const int n = INTEGER_VALUE(N);
  
  Tnode *NGramA = NULL;
  Tnode *NGramB = NULL;
  int gram_countA, gram_countB;
  SEXP result;


  NGramA = createTree(NGramA, myStrA, n, &gram_countA);
  NGramB = createTree(NGramB, myStrB, n, &gram_countB);

  double score;
  if (gram_countA == 0 || gram_countB == 0){
    score = 1;
  } else if (gram_countA <= gram_countB){
    score = scoreTree(NGramA, NGramB) / (4 * gram_countA);
  } else {
    score = scoreTree(NGramB, NGramA) / (4 * gram_countB);
  }
  
//  printf("%f\n", score);
  
  freeTree(NGramA);
  freeTree(NGramB);
  
  PROTECT(result = NEW_NUMERIC(1));
  REAL(result)[0] = score;
  UNPROTECT(1);
  return (result);
}

SEXP NGramWords(SEXP strA, SEXP strB, SEXP N){
  const char *myStrA = CHAR(STRING_ELT(strA,0));
  const char *myStrB = CHAR(STRING_ELT(strB,0));
  const int n = INTEGER_VALUE(N);
  
  Tnode *NGramA = NULL;
  Tnode *NGramB = NULL;
  int gram_countA, gram_countB;
  SEXP result;

  TokenNode *wordListA = NULL;
  TokenNode *wordListB = NULL;
  wordListA = createTokenList(wordListA, myStrA);
  wordListB = createTokenList(wordListB, myStrB);  
  
  int sentenceSize = (strlen(myStrA)+1)*sizeof(char);
  NGramA = createWordTree(NGramA, wordListA, n, sentenceSize, &gram_countA);
  freeTokenList(wordListA);
  
  sentenceSize = (strlen(myStrB)+1)*sizeof(char);
  NGramB = createWordTree(NGramB, wordListB, n, sentenceSize, &gram_countB);
  freeTokenList(wordListB);
  
  double score;
  if (gram_countA == 0 || gram_countB == 0){
      score = 1;
    } else if (gram_countA <= gram_countB){
    score = scoreTree(NGramA, NGramB) / (4 * gram_countA);
  } else {
    score = scoreTree(NGramB, NGramA) / (4 * gram_countB);
  }
  

  freeTree(NGramA);
  freeTree(NGramB);
  
  PROTECT(result = NEW_NUMERIC(1));
  REAL(result)[0] = score;
  UNPROTECT(1);
  return (result);
}

double scoreTree(Tnode *treeA, Tnode *treeB){
  if(treeA == NULL){
    return 0;
  }
  
  double myScore = treeA->score;
  double bScore = scoreLookup(treeB, treeA->ngram);
  
  double returnScore = (2 * (myScore - bScore) / (myScore + bScore));
  returnScore = returnScore * returnScore + scoreTree(treeA->lhs, treeB) + scoreTree(treeA->rhs, treeB);
  
  return (returnScore);
}

double scoreLookup(Tnode *tree, int gram){
  if(tree == NULL){
    return 0;
  }
  
  int cond;
  
  if ((cond = (gram - tree->ngram)) == 0){
    return tree->score;
  } else if (cond < 0){
    return scoreLookup(tree->lhs, gram);
  } else {
    return scoreLookup(tree->rhs, gram);
  }
}

void fixScore(Tnode *tree, int tot_count){
  if(tree == NULL || tot_count <= 0){
    return;
  }
  
  tree->score = 1.0 * tree->count / tot_count;
  fixScore(tree->lhs, tot_count);
  fixScore(tree->rhs, tot_count);
  
  return;
}

Tnode *createTree(Tnode *tree, const char* word, const int n, int *gram_count){
  char *buff = NULL;
  
  buff = (char*) malloc (n + 1);
  buff[n] = '\0';
  int i;
  
  for(i = 0; i <= strlen(word) - n && i < strlen(word); i++){
    strncpy(buff, word + i, n);
    tree = addTree(tree, fnv1a(buff, Seed));
  }
  fixScore(tree, i);
  *gram_count = i;

  free(buff);
  return tree;
}

Tnode *addTree(Tnode *tree, int gram){
  int cond;
  
  if (tree == NULL){
    tree = treeAlloc();
    tree->ngram = gram;
    tree->count = 1;
    tree->lhs = tree->rhs = NULL;
  } else if ((cond = (gram - tree->ngram)) == 0){
    tree->count++;
  } else if (cond < 0){
    tree->lhs = addTree(tree->lhs, gram);
  } else {
    tree->rhs = addTree(tree->rhs, gram);
  }
  
  return tree;
}

Tnode *treeAlloc(void){
  return (Tnode *) malloc (sizeof(Tnode));
}

void freeTree(Tnode *tree){
  if (tree == NULL){
    return;
  }
  
  if (tree->lhs != NULL){
    freeTree(tree->lhs);
  }
  
  if (tree->rhs != NULL){
    freeTree(tree->rhs);
  }
  
  free(tree);
  
  return;
}

void printTree(Tnode *tree){
  if (tree != NULL){
    printTree(tree->lhs);
    printf("%i %i %f\n", tree->ngram, tree->count, tree->score);
    printTree(tree->rhs);
  }
  return;
}
