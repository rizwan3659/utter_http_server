/*
 * utter.c - [Starting code for] a web-based Ute social network.
 *
 * Based on:
 *  tiny.c - A simple, iterative HTTP/1.0 Web server that uses the
 *      GET method to serve static and dynamic content.
 *   Tiny Web server
 *   Dave O'Hallaron
 *   Carnegie Mellon University
 */
#include "csapp.h"
#include "dictionary.h"
#include "more_string.h"

#define MAXT_IN_POOL 200

typedef struct task_st {
    void (*routine) (void *);
    void *arg;
    struct task_st* next;
} task_t;

typedef void *threadpool;


typedef void (*dispatch_fn)(void *);

threadpool create_threadpool(int num_threads_in_pool);

void dispatch(threadpool from_me, dispatch_fn dispatch_to_here,
	      void *arg);

void destroy_threadpool(threadpool destroyme);
static void doit(int fd);
static dictionary_t *read_requesthdrs(rio_t *rp);
static void read_postquery(rio_t *rp, dictionary_t *headers, dictionary_t *d);
static void clienterror(int fd, char *cause, char *errnum,
                        char *shortmsg, char *longmsg);
static void print_stringdictionary(dictionary_t *d);

//static void serve_request(int fd, dictionary_t *query);
 int serve_utter(int fd, char *user , char *message);
 void serve_shh(int fd, char *user , char *id);
 void serve_sync(int fd ,char *hostname , char *port, char *user);
 void serve_listen(int fd, char *user );

dictionary_t *users ;

char port[128];

typedef struct _threadpool_st {
   int available_threads;
   pthread_mutex_t mutex;
   pthread_cond_t q_empty;
   pthread_cond_t q_not_empty;
   pthread_t *threads;
   int tsize;
   task_t *thead;
   task_t *ttail;
} _threadpool;

#ifdef SILENCE_PRINTF
#define printf(...)
#endif


int main(int argc, char **argv) {

  int listenfd, connfd;
  char hostname[MAXLINE], port[MAXLINE];
  socklen_t clientlen;
  struct sockaddr_storage clientaddr;

  /* Check command line args */
  if (argc != 2) {
    fprintf(stderr, "usage: %s <port>\n", argv[0]);
    exit(1);
  }

  strcpy(port,argv[1]);
 // users = make_dictionary(COMPARE_CASE_SENS, free);
  listenfd = Open_listenfd(argv[1]);

  /* Don't kill the server if there's an error, because
     we want to survive errors due to a client. But we
     do want to report errors. */
  exit_on_error(0);

  /* Also, don't stop on broken connections: */
  Signal(SIGPIPE, SIG_IGN);

 
  threadpool tp;
  tp = create_threadpool(199);

 while (1) {
    clientlen = sizeof(clientaddr);
    connfd = Accept(listenfd, (SA *)&clientaddr, &clientlen);
    if (connfd >= 0) {
      Getnameinfo((SA *) &clientaddr, clientlen, hostname, MAXLINE,
          port, MAXLINE, 0);
      printf("Accepted connection from (%s, %s)\n", hostname, port);
    dispatch(tp, doit, (void *)connfd);
    }
  }








}

/*
 * doit - handle one HTTP request/response transaction
 */
void doit(int fd){

  char buf[MAXLINE], *method, *uri, *version;
//  char hostname[MAXLINE], port[MAXLINE];
  rio_t rio;
  dictionary_t *headers, *query;
    char *user,*message,*id,*port1,*hostname1;
  /* Read request line and headers */
  Rio_readinitb(&rio, fd);
  if (Rio_readlineb(&rio, buf, MAXLINE) <= 0)
    return;

  if (!parse_request_line(buf, &method, &uri, &version)) {
    clienterror(fd, method, "400", "Bad Request",
                "Utter did not recognize the request");
  } else {
    if (strcasecmp(version, "HTTP/1.0")
        && strcasecmp(version, "HTTP/1.1")) {
      clienterror(fd, version, "501", "Not Implemented",
          "Utter does not implement that version");
    } else if (strcasecmp(method, "GET")
        && strcasecmp(method, "POST")){
      clienterror(fd, method, "501", "Not Implemented",
          "Utter does not implement that method");
    } else {
      headers = read_requesthdrs(&rio);

      /* Parse all query arguments into a dictionary */
      query = make_dictionary(COMPARE_CASE_SENS, free);
      printf(" URI %s \n", uri );
      parse_uriquery(uri, query);
      if (!strcasecmp(method, "POST")){
        read_postquery(&rio, headers, query);
      }
      /*For debugging, print the dictionary */
      printf("query params\n");
      print_stringdictionary(query);
      printf("URI: %s\n", uri);


      /* You'll want to handle different queries here,
         but the intial implementation always returns
         "hello world": */
 
     if(!strncmp(uri,"/listen",7))
      {
        print_stringdictionary(query);
        user = dictionary_get(query,"user");
      if(user!=NULL)
        serve_listen(fd, user);
      }
     else if(!strncmp(uri,"/utter",5))
      {
     // print_stringdictionary(query);
        user = dictionary_get(query,"user");
        message = dictionary_get(query,"utterance");
      if(user != NULL && message != NULL)
   {
        if(serve_utter(fd, user,message) ==-1)
         {
      clienterror(fd, method, "503", "Service Unavailable",
          "Hostname does not exist");
         
         }
}
      }
     else if(!strncmp(uri,"/shh",3))
      {
        user = dictionary_get(query,"user");
        id = dictionary_get(query,"id");
        printf("%s",id);
       if(user && id )
        serve_shh(fd, user,id);
      }
     else if(!strncmp(uri,"/sync",4))
      {
      print_stringdictionary(query);
        port1 = dictionary_get(query,"port");
        hostname1 = dictionary_get(query,"hostname");
        user = dictionary_get(query,"user");
      if(hostname1&& port1 && user)
        serve_sync(fd ,hostname1,port1,user );
      }

      /* Clean up */
      free_dictionary(query);
      free_dictionary(headers);
    }

    /* Clean up status line */
    free(method);
    free(uri);
    free(version);

  }

}

/*
 * read_requesthdrs - read HTTP request headers
 */
dictionary_t *read_requesthdrs(rio_t *rp) {
  char buf[MAXLINE];
  dictionary_t *d = make_dictionary(COMPARE_CASE_INSENS, free);

  Rio_readlineb(rp, buf, MAXLINE);
  while(strcmp(buf, "\r\n")) {
    Rio_readlineb(rp, buf, MAXLINE);
    parse_header_line(buf, d);
  }

  return d;
}

void read_postquery(rio_t *rp, dictionary_t *headers, dictionary_t *dest) {
  char *len_str, *type, *buffer;
  int len;

  len_str = dictionary_get(headers, "Content-Length");
  len = (len_str ? atoi(len_str) : 0);

  type = dictionary_get(headers, "Content-Type");

  buffer = malloc(len+1);
  Rio_readnb(rp, buffer, len);
  buffer[len] = 0;

  if (!strcasecmp(type, "application/x-www-form-urlencoded")) {
    parse_query(buffer, dest);
  }

  free(buffer);
}

static char *ok_header(size_t len, const char *content_type) {
  char *len_str, *header;

  header = append_strings("HTTP/1.0 200 OK\r\n",
                          "Server: Utter Web Server\r\n",
                          "Connection: close\r\n",
                          "Content-length: ", len_str = to_string(len), "\r\n",
                          "Content-type: ", content_type, "\r\n\r\n",
                          NULL);
  free(len_str);

  return header;
}



void serve_listen(int fd, char *user) {
  size_t len;
  char body[MAXBUF], *header;
  dictionary_t *messages;
user = strdup(user);
if(users == NULL)
   {

      sprintf(body,"There is no utter for this %s user",user);
   }

 messages = dictionary_get(users , user);
 if(messages != NULL)
 {
  int i, count;
 int k=0;
  count = dictionary_count(messages);

  if(count == 0)
{

      sprintf(body,"There is no utter for %s user",user);

  print_stringdictionary(messages);;
}
else
{
  for (i = 0; i < count; i++) {
    sprintf(body + k,"%s %s\n",
            dictionary_key(messages,i),
           (const char *)dictionary_value(messages, i));
    k=strlen(body);
  }
 }
}
else
  {
      sprintf(body,"There is no utter for %s user",user);
  }


  len = strlen(body);
  /* Send response headers to client */
  header = ok_header(len, "text/plain; charset=utf-8");
  Rio_writen(fd, header, strlen(header));
  printf("Response headers:\n");
  printf("%s", header);

  free(header);

  /* Send response body to client */
  Rio_writen(fd, body, len);
  Close(fd);
   free(user);
}

 void serve_shh(int fd, char *user, char *id) {
  char  *header;
  dictionary_t *messages;

   user = strdup(user);
   id = strdup(id);

if(users == NULL)
{
  goto response;
}
else
{
   messages = dictionary_get(users , user);
}

 if(messages != NULL)
 {
  printf("Before dictionary");
  print_stringdictionary(messages);;
    dictionary_remove(messages,id) ;
   if(dictionary_count(messages)==0)
   {
      free(messages);
    } 
  }
else
  {
  ; 
  }


response: 
 header = ok_header(0, "text/plain; charset=utf-8");
  Rio_writen(fd, header, strlen(header));
  printf("Response headers:\n");
  printf("%s", header);

  free(header);

   free(user);
  free(id);
}




void serve_sync(int fd ,char *hostname, char *port,  char *user)
 {

  char *header;
    rio_t rio;
  dictionary_t *messages;
    char url[128];
    char request_buffer[MAXBUF] ;
    char buf[MAXBUF];
    int sockfd;
    struct addrinfo hints;
    struct addrinfo *res;  

    user=strdup(user);
    hostname=strdup(hostname);
    port=strdup(port);

    memset(&hints, 0, sizeof(hints)); 
    hints.ai_family = AF_UNSPEC;     
    hints.ai_socktype = SOCK_STREAM; 



    Getaddrinfo(hostname, port, &hints, &res);

    sockfd = Socket(res->ai_family, res->ai_socktype, res->ai_protocol);
  

    
   Connect(sockfd, res->ai_addr, res->ai_addrlen);


    Freeaddrinfo(res);

   sprintf(url,"/listen?user=%s",user);
   sprintf(request_buffer, "GET %s HTTP/1.0\r\nHost: %s:%s\r\n\r\n",url, hostname, port );
  Rio_writen(sockfd, request_buffer, strlen(request_buffer));

  Rio_readinitb(&rio, sockfd);
  if (Rio_readlineb(&rio, buf, MAXLINE) <= 0)
       return ;


    if (strncmp("HTTP/1.0 ", buf, 9) != 0 && strncmp("HTTP/1.1 ", buf, 9) != 0) {
	fprintf(stderr, "unknown protocol response: %s\n", buf);
	exit(1);
    }
    if (strncmp("200", buf + 9, 3) != 0) {
	fprintf(stderr, "%s\n", buf);
	exit(1);
    }

    for (;;) {
              if (Rio_readlineb(&rio, buf, MAXLINE) <= 0)
                 {
	            return;
                 }
	if (strcmp("\r\n", buf) == 0) {
	    break;
	}
    }


  //    query = make_dictionary(COMPARE_CASE_SENS, free);
char body[MAXBUF], message[MAXBUF];
    size_t k;
    while((k = Rio_readlineb(&rio,buf,MAXLINE)) > 0) 
   {
  if(users == NULL)
   {
     users = make_dictionary(COMPARE_CASE_SENS, free);
   }
else 
  {
     messages = dictionary_get(users , user);
   }
 if(messages != NULL)
 {
  printf("%s",buf);
  sscanf(buf,"%s %s",body,message);
 dictionary_set(messages,body,message) ;
  }
else
  {
    messages = make_dictionary(COMPARE_CASE_SENS, free);
      dictionary_set(users ,user,  messages); 
  sscanf(buf,"%s %s",body,message);
 dictionary_set(messages,body,message) ;

  }
   }


//  print_stringdictionary(messages);;

  /* Send response headers to client */
  header = ok_header(0, "text/plain; charset=utf-8");
  Rio_writen(fd, header, strlen(header));
  printf("Response headers:\n");
  printf("%s", header);

  free(header);
  free(hostname);
  free(port);
  /* Send response body to client */
//  Rio_writen(fd, body, len);

}






int serve_utter(int fd, char *user, char *message) {
  size_t len;
  int count;
  char body[MAXBUF], *header;
  dictionary_t *messages =NULL ;
  char hostname[128];
   int rc=0;

   int ret=1;

user=strdup(user);
message=strdup(message);


ret= gethostname(hostname,128);
if (ret == -1)
    return ret;
  
if(users == NULL)
   {
     users = make_dictionary(COMPARE_CASE_SENS, free);
   }
else 
  {
     messages = dictionary_get(users , user);
   }


   if(messages != NULL)
 {
  count = dictionary_count(messages);
  
  sprintf(body,"%s_%s_%u",hostname,port,count+1);
 dictionary_set(messages,body,message) ;
  print_stringdictionary(messages);;
  }
else
  {
    messages = make_dictionary(COMPARE_CASE_SENS, free);
      dictionary_set(users ,user,  messages); 
  sprintf(body,"%s_%s_1",hostname,port);
  dictionary_set(messages,body,message) ;
  print_stringdictionary(messages);;

  }



  print_stringdictionary(users);;
//  print_stringdictionary(messages);;
  len = strlen(body);

  /* Send response headers to client */
  header = ok_header(len, "text/plain; charset=utf-8");
  Rio_writen(fd, header, strlen(header));
  printf("Response headers:\n");
  printf("%s", header);

  free(header);
  // free(message);
  /* Send response body to client */
  Rio_writen(fd, body, len);

  return rc;
//  free(body);
}
/*
 * clienterror - returns an error message to the client
 */
void clienterror(int fd, char *cause, char *errnum,
		 char *shortmsg, char *longmsg) {
  size_t len;
  char *header, *body, *len_str;

  body = append_strings("<html><title>Utter Error</title>",
                        "<body bgcolor=""ffffff"">\r\n",
                        errnum, " ", shortmsg,
                        "<p>", longmsg, ": ", cause,
                        "<hr><em>Utter Server</em>\r\n",
                        NULL);
  len = strlen(body);

  /* Print the HTTP response */
  header = append_strings("HTTP/1.0 ", errnum, " ", shortmsg, "\r\n",
                          "Content-type: text/html; charset=utf-8\r\n",
                          "Content-length: ", len_str = to_string(len), "\r\n\r\n",
                          NULL);
  free(len_str);

  Rio_writen(fd, header, strlen(header));
  Rio_writen(fd, body, len);

  free(header);
}

static void print_stringdictionary(dictionary_t *d) {
  int i, count;

  count = dictionary_count(d);
  for (i = 0; i < count; i++) {
    printf(" key value %s=%s\n",
           dictionary_key(d, i),
           (const char *)dictionary_value(d, i));
  }
  printf("\n");
}



void *thread_work(threadpool ptemp)
{
  _threadpool *pool = (_threadpool *)ptemp;
  task_t *current;   

  pthread_mutex_lock(&(pool->mutex));

  while (1) {
     while(pool->tsize == 0)
     {
         pthread_cond_wait(&(pool->q_not_empty), &(pool->mutex));
     }
  
     current = pool->thead;
     pool->tsize--;
     if (pool->tsize == 0) {
       pool->thead = NULL;
       pool->ttail = NULL;
     } 
     else
     {
        pool->thead = current->next;
     }

    /*  
    if (pool->tsize == 0)
    {
       pthread_cond_signal(&(pool->q_empty));
    }
    */

    //pool->available_threads--;
    pthread_mutex_unlock(&(pool->mutex)); 
    (current->routine) (current->arg);

    /*
    pthread_mutex_lock(&(pool->mutex));
    pool->available_threads++; 
    pthread_cond_signal(&(pool->q_empty));
    pthread_mutex_unlock(&(pool->mutex));
    */

    free(current);
  }
}

threadpool create_threadpool(int num_threads_in_pool) {

  _threadpool *pool;
  int i;

  // sanity check the argument
  if ((num_threads_in_pool <= 0) || (num_threads_in_pool > MAXT_IN_POOL))
    return NULL;

  pool = (_threadpool *) malloc(sizeof(_threadpool));
  if (pool == NULL) {
    fprintf(stderr, "Out of memory creating a new threadpool!\n");
    return NULL;
  }

  // add your code here to initialize the newly created threadpool
  pool->available_threads = num_threads_in_pool;
  pool->tsize = 0;
  pool->thead = NULL;
  pool->ttail = NULL;
  pool->threads = (pthread_t *)malloc(num_threads_in_pool * sizeof(pthread_t));
  pthread_mutex_init(&(pool->mutex), NULL);
  pthread_cond_init(&(pool->q_empty), NULL);
  pthread_cond_init(&(pool->q_not_empty), NULL);

  for (i=0; i< pool->available_threads ; i++)
  {
     if(pthread_create(&(pool->threads[i]), NULL, thread_work, pool) != 0)
     {
        fprintf(stderr, "Error during threadpool creation!\n");  
        return NULL;
     }
  }

  return (threadpool) pool;
}


void dispatch(threadpool from_me, dispatch_fn dispatch_to_here,
	      void *arg) {
  _threadpool *pool = (_threadpool *) from_me;

  // add your code here to dispatch a thread
  //printf("Inside dispatch\n");
  task_t *cur_task;
  
  cur_task = (task_t *)malloc(sizeof(task_t));
  if (cur_task == NULL)
  {
      fprintf(stderr, "Error allocating space for task\n");
      return;
  }

  cur_task->routine = dispatch_to_here;
  cur_task->arg = arg;
  cur_task->next = NULL;

  pthread_mutex_lock(&(pool->mutex));
   
  if (pool->tsize == 0)
  {
     pool->tsize++;
     pool->thead = cur_task;
     pool->ttail = cur_task;
     pthread_cond_signal(&(pool->q_not_empty));
     //pthread_cond_wait(&(pool->q_empty), &(pool->mutex));
  }
  else
  {
     pool->tsize++;
     pool->ttail->next = cur_task;
     pool->ttail = cur_task;
     pthread_cond_signal(&(pool->q_not_empty));
     //pthread_cond_wait(&(pool->q_empty), &(pool->mutex));
  }

  /*
  while(pool->available_threads == 0)
  {
     //pthread_cond_signal(&(pool->q_not_empty));
     pthread_cond_wait(&(pool->q_empty), &(pool->mutex));
  }
  */

  pthread_mutex_unlock(&(pool->mutex));

}
/*
  pthread_mutex_lock(&(pool->mutex));
  printf("After pthread_mutex_lock\n");
  if (!pool->available_threads)
  {
      pthread_cond_wait(&(pool->cond_var), &(pool->mutex));
  }
  printf("After pthread_cond_wait\n"); 
  pool->available_threads--;
  printf("Available threads = %d", pool->available_threads);
  pthread_mutex_unlock(&(pool->mutex)); 
  pthread_create(*(pool->threads + pool->available_threads), NULL, (void *)dispatch_to_here , arg);
  return;
}
*/

void destroy_threadpool(threadpool destroyme) {
  _threadpool *pool = (_threadpool *) destroyme;

  // add your code here to kill a threadpool
  pthread_mutex_destroy(&(pool->mutex));
  pthread_cond_destroy(&(pool->q_empty));
  pthread_cond_destroy(&(pool->q_not_empty));
  pool->available_threads = 0;
  free(pool->threads);
  free(pool);
  return; 
}

