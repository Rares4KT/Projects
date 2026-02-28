#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <netdb.h>
#include <string.h>
#include <sys/select.h> 

extern int errno;

int port;

int main(int argc, char *argv[])
{
  int sd;                    
  struct sockaddr_in server; 
  char buffer[4096]; 

  if (argc != 3)
  {
    printf("sintaxa: %s <adresa_server> <port>\n", argv[0]);
    return -1;
  }

  port = atoi(argv[2]);

  if ((sd = socket(AF_INET, SOCK_STREAM, 0)) == -1)
  {
    perror("eroare la socket().\n");
    return errno;
  }

  server.sin_family = AF_INET;
  server.sin_addr.s_addr = inet_addr(argv[1]);
  server.sin_port = htons(port);

  if (connect(sd, (struct sockaddr *)&server, sizeof(struct sockaddr)) == -1)
  {
    perror("eroare la connect().\n");
    return errno;
  }

  fd_set read_fds;    
  fd_set tmp_fds;     
  int fdmax;          

  FD_ZERO(&read_fds);
  FD_SET(0, &read_fds); 
  FD_SET(sd, &read_fds); 

  fdmax = sd; 

  while(1) {
      tmp_fds = read_fds; 

      if (select(fdmax + 1, &tmp_fds, NULL, NULL, NULL) == -1) {
          perror("eroare la select()");
          break;
      }

      if (FD_ISSET(0, &tmp_fds)) {
          memset(buffer, 0, 4096);
          int bytes_read = read(0, buffer, 4095);
          
          if(bytes_read < 0) {
              perror("eroare la read tastatura");
              break;
          }
          
          if (write(sd, buffer, strlen(buffer)) <= 0) {
              perror("eroare la write() spre server.\n");
              break;
          }

          if(strncmp(buffer, "QUIT", 4) == 0) {
              printf("Inchidere client...\n");
              break;
          }
      }

      if (FD_ISSET(sd, &tmp_fds)) {
          memset(buffer, 0, 4096);
          int bytes_read = read(sd, buffer, 4095);

          if (bytes_read <= 0) {
              printf("\n[client] Serverul a inchis conexiunea.\n");
              break;
          }

          printf("%s", buffer);
          fflush(stdout); 

          if (strstr(buffer, "Deconectare") != NULL || 
              strstr(buffer, "La revedere") != NULL ||
              strstr(buffer, "ACK_QUIT") != NULL) {
              
              printf("\n[client] Sesiune incheiata cu succes.\n");
              break; 
          }
      }
  }

  close(sd);
  return 0;
}