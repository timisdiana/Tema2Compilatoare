%{
	#include <stdio.h>
        #include <string.h>

	int yylex();
	int yyerror(const char *msg);

     int EsteCorecta = 1;
	char msg[500];

	class TVAR
	{
	     char* nume;
	     int valoare;
	     TVAR* next;
	  
	  public:
	     static TVAR* head;
	     static TVAR* tail;

	     TVAR(char* n, int v = -1);
	     TVAR();
	     int exists(char* n);
             void add(char* n, int v = -1);
             int getValue(char* n);
	     void setValue(char* n, int v);
	};

	TVAR* TVAR::head;
	TVAR* TVAR::tail;

	TVAR::TVAR(char* n, int v)
	{
	 this->nume = new char[strlen(n)+1];
	 strcpy(this->nume,n);
	 this->valoare = v;
	 this->next = NULL;
	}

	TVAR::TVAR()
	{
	  TVAR::head = NULL;
	  TVAR::tail = NULL;
	}

	int TVAR::exists(char* n)
	{
	  TVAR* tmp = TVAR::head;
	  while(tmp != NULL)
	  {
	    if(strcmp(tmp->nume,n) == 0)
	      return 1;
            tmp = tmp->next;
	  }
	  return 0;
	 }

         void TVAR::add(char* n, int v)
	 {
	   TVAR* elem = new TVAR(n, v);
	   if(head == NULL)
	   {
	     TVAR::head = TVAR::tail = elem;
	   }
	   else
	   {
	     TVAR::tail->next = elem;
	     TVAR::tail = elem;
	   }
	 }

         int TVAR::getValue(char* n)
	 {
	   TVAR* tmp = TVAR::head;
	   while(tmp != NULL)
	   {
	     if(strcmp(tmp->nume,n) == 0)
	      return tmp->valoare;
	     tmp = tmp->next;
	   }
	   return -1;
	  }

	  void TVAR::setValue(char* n, int v)
	  {
	    TVAR* tmp = TVAR::head;
	    while(tmp != NULL)
	    {
	      if(strcmp(tmp->nume,n) == 0)
	      {
		tmp->valoare = v;
	      }
	      tmp = tmp->next;
	    }
	  }

	TVAR* ts = NULL;
%}





%union { char* sir; int val; }

%token TOK_PROGRAM TOK_BEGIN TOK_END
%token TOK_PLUS TOK_MINUS TOK_MULTIPLY TOK_DIV TOK_LEFT TOK_RIGHT TOK_DECLARE TOK_PRINT TOK_EQU TOK_FOR TOK_DO TOK_TO TOK_ERROR
%token TOK_READ TOK_WRITE TOK_ATTRIB TOK_INTEGER  

%token <val> TOK_INT
%token <sir> TOK_ID

%type <sir> IDLIST READ
%type <val> FACTOR TERM EXP



%start PROG

%left TOK_PLUS TOK_MINUS
%left TOK_MULTIPLY TOK_DIVIDE
%locations 
%%
PROG : 
      
  TOK_PROGRAM PROGRNAME TOK_DECLARE DECLIST TOK_BEGIN STMTLIST TOK_END
	
       |
       error  { EsteCorecta=0; };

PROGRNAME : TOK_ID          
           {
               
           }
                ;

DECLIST : 
          DEC 
          |
          DECLIST ';' DEC 
       {
         
       }

;

DEC :  IDLIST TOK_ATTRIB TYPE 
          {
            
	  if(ts!=NULL)
          {
		char *s;
		s=strtok($1, ",");
		while (s != NULL)
 		 {
   		    if(ts->exists(s)==0)
             		 {
	                   ts->add(s);
             		 }
            	    else
              	        {
                	   sprintf(msg, "%d:%d Eroare semantica: Declaratii multiple pentru variabila %s!", @1.first_line, @1.first_column, s);
               		   yyerror(msg);
                	   YYERROR;
              		}
      
   s = strtok (NULL, ",");
                }
               return 0;
          }
             
         else 
          {
            ts=new TVAR();
            ts->add($1);
             
          } 
        
          };

TYPE : TOK_INTEGER ;

IDLIST : 
        TOK_ID
        
        |
        IDLIST ',' TOK_ID
	{
	   strcat($$,",");
           strcat($$,$3);
           
        }
       
     ;
STMTLIST : 
          STMT
          |
          STMTLIST ';' STMT  ;
         

STMT : ASSIGN
       |
       READ
       |
       WRITE
       |
       FOR ;



ASSIGN : 
         TOK_ID TOK_EQU EXP 
          {
	     if(ts!=NULL)
             {
               if(ts->exists($1)==1)
                {
                
                 ts->setValue($1, $3);

                }
                 else
                 {
                  sprintf(msg, "%d:%d Eroare semantica: Variabila %s este utilizata fara sa fi fost initializata!", @1.first_line, @1.first_column, $1);
                   yyerror(msg);
                   YYERROR;
                  }
                }
             }
            
          
          ;

EXP : 
      TERM
      |
      EXP TOK_PLUS TERM { $$ = $1 + $3; }

      | 
      EXP TOK_MINUS TERM {$$ = $1 - $3; };



TERM : 
       FACTOR
       |
       TERM TOK_MULTIPLY FACTOR {$$ = $1 * $3; };
       |
       TERM TOK_DIV FACTOR 
	{
		if($3==0)
		{
			sprintf(msg, "%d:%d Eroare semantica: Impartire la zero!", @1.first_line, @1.first_column);
			yyerror(msg);
			YYERROR; 
		}
		else { $$ = $1 / $3; }
	}
;

FACTOR :        
         TOK_LEFT EXP TOK_RIGHT { $$ = $2; }
         | 
         TOK_ID
          {
             if(ts!=NULL)
             {
               if(ts->exists($1)==0)
                {
                  sprintf(msg, "%d:%d Eroare semantica: Variabila %s este utilizata fara sa fi fost declarata!", @1.first_line, @1.first_column, $1);
                   yyerror(msg);
                   YYERROR;
                } 
               

             }
            else
              {
                  sprintf(msg, "%d:%d Eroare semantica: Variabila %s este utilizata fara sa fi fost declarata!", @1.first_line, @1.first_column, $1);
                   yyerror(msg);
                   YYERROR;
                } 
                 
               
          }
         |
         TOK_INT { $$ = $1; } ;

READ : TOK_READ TOK_LEFT IDLIST TOK_RIGHT 
	{
   		
        }

;

WRITE : TOK_WRITE TOK_LEFT IDLIST TOK_RIGHT ;

FOR : TOK_FOR INDEXEXP TOK_DO BODY ;

INDEXEXP : TOK_ID TOK_EQU EXP TOK_TO EXP 
            {
             if(ts!=NULL)
             {
               if(ts->exists($1)==0)
                {
                  sprintf(msg, "%d:%d Eroare semantica: Variabila %s este utilizata fara sa fi fost declarata!", @1.first_line, @1.first_column, $1);
                   yyerror(msg);
                   YYERROR;
                }
             }
              else

               {
                  sprintf(msg, "%d:%d Eroare semantica: Variabila %s este utilizata fara sa fi fost declarata!", @1.first_line, @1.first_column, $1);
                   yyerror(msg);
                   YYERROR;
                }

          }
;

BODY : STMT
       |
       TOK_BEGIN STMTLIST TOK_END ';' ;

   






%%

int main()
{
	yyparse();
	
	if(EsteCorecta == 1)
	{
		printf("CORECTA\n");		
	}
        if(EsteCorecta==0)
        {
		printf("Gresita\n");
        }	

       return 0;
}

int yyerror(const char *msg)
{
	printf("Error: %s\n", msg);
	return 1;
}
