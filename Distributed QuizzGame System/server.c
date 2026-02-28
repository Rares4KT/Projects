#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <pthread.h>
#include <sqlite3.h>

#define PORT 2908
#define BUFF_SIZE 4096
#define TIMP_RASPUNS 10  
#define MIN_JUCATORI 2   

extern int errno;

typedef struct {
    char text[256];
    char variantaA[100];
    char variantaB[100];
    char variantaC[100];
    char raspuns_corect[10];
} Intrebare;

typedef struct {
    int socket;
    char username[50];
    int scor;
    int este_activ;    
    int id_intern;
    char raspuns_curent[10];
} Jucator;

Intrebare set_intrebari[100];
int nr_intrebari = 0;

Jucator clienti[100];
int max_clienti_index = 0; 

int joc_inceput = 0; 
int runda_curenta = -1; 

pthread_mutex_t mlock = PTHREAD_MUTEX_INITIALIZER;

void incarca_intrebari_din_db(const char* nume_db) {
    sqlite3 *db;
    sqlite3_stmt *res;
    int rc = sqlite3_open(nume_db, &db);
    if (rc != SQLITE_OK) { perror("Eroare deschidere DB"); exit(1); }

    const char *sql = "SELECT text, vA, vB, vC, corect FROM intrebari";
    sqlite3_prepare_v2(db, sql, -1, &res, 0);

    nr_intrebari = 0;
    while (sqlite3_step(res) == SQLITE_ROW) {
        strcpy(set_intrebari[nr_intrebari].text, (char*)sqlite3_column_text(res, 0));
        strcpy(set_intrebari[nr_intrebari].variantaA, (char*)sqlite3_column_text(res, 1));
        strcpy(set_intrebari[nr_intrebari].variantaB, (char*)sqlite3_column_text(res, 2));
        strcpy(set_intrebari[nr_intrebari].variantaC, (char*)sqlite3_column_text(res, 3));
        strcpy(set_intrebari[nr_intrebari].raspuns_corect, (char*)sqlite3_column_text(res, 4));
        nr_intrebari++;
    }
    sqlite3_finalize(res);
    sqlite3_close(db);
    printf("[SERVER] DB incarcat: %d intrebari.\n", nr_intrebari);
}

void trimite_tuturor(char *mesaj) {
    pthread_mutex_lock(&mlock);
    for (int i = 0; i < max_clienti_index; i++) {
        if (clienti[i].este_activ) {
            write(clienti[i].socket, mesaj, strlen(mesaj));
        }
    }
    pthread_mutex_unlock(&mlock);
}

int numara_jucatori_activi() {
    int count = 0;
    for(int i=0; i<max_clienti_index; i++) {
        if(clienti[i].este_activ && strcmp(clienti[i].username, "Anonim") != 0) {
            count++;
        }
    }
    return count;
}

void *game_logic(void *arg) {
    
    while(1) {

        printf("[GAME] Initializare runda noua...\n");
        joc_inceput = 0;
        runda_curenta = -1;
        
        pthread_mutex_lock(&mlock);
        for(int i=0; i<max_clienti_index; i++) {
            if(clienti[i].este_activ) {
                clienti[i].scor = 0;
                bzero(clienti[i].raspuns_curent, 10);
            }
        }
        pthread_mutex_unlock(&mlock);

        printf("[GAME] Asteptam jucatori (Minim %d)...\n", MIN_JUCATORI);
        while (1) {
            pthread_mutex_lock(&mlock);
            int gata = numara_jucatori_activi();
            if (gata >= MIN_JUCATORI) {
                joc_inceput = 1;
                pthread_mutex_unlock(&mlock);
                break;
            }
            pthread_mutex_unlock(&mlock);
            sleep(1);
        }

        char buffer[BUFF_SIZE];
        snprintf(buffer, BUFF_SIZE, "\n--- JOCUL A INCEPUT! ---\nPregatiti-va...\n\n");
        trimite_tuturor(buffer);
        sleep(3);

        int joc_intrerupt = 0;

        for (int i = 0; i < nr_intrebari; i++) {
            pthread_mutex_lock(&mlock);
            if (numara_jucatori_activi() < MIN_JUCATORI) {
                pthread_mutex_unlock(&mlock); 
                printf("[GAME] Nu mai sunt suficienti jucatori. Restartam lobby-ul.\n");
                trimite_tuturor("\n[!] Prea multi jucatori au iesit. Jocul se anuleaza.\n");
                joc_intrerupt = 1;
                break;
            }
            pthread_mutex_unlock(&mlock);

            runda_curenta = i;
            
            for(int j=0; j<max_clienti_index; j++) bzero(clienti[j].raspuns_curent, 10);

            bzero(buffer, BUFF_SIZE);
            snprintf(buffer, BUFF_SIZE, "INTREBAREA %d:\n%s\nA. %s\nB. %s\nC. %s\n\nAveti %d secunde!\n", 
                    i+1, set_intrebari[i].text, 
                    set_intrebari[i].variantaA, set_intrebari[i].variantaB, set_intrebari[i].variantaC,
                    TIMP_RASPUNS);
            
            trimite_tuturor(buffer);
            printf("[GAME] Runda %d a inceput.\n", i+1);

            int stop_fortat = 0;
            for(int k=0; k < TIMP_RASPUNS; k++) {
                sleep(1); 
                
                pthread_mutex_lock(&mlock);
                if (numara_jucatori_activi() < MIN_JUCATORI) {
                    stop_fortat = 1;
                    pthread_mutex_unlock(&mlock);
                    break; 
                }
                pthread_mutex_unlock(&mlock);
            }

            if (stop_fortat) {
                printf("[GAME] Jucator deconectat in timpul numaratorii. Anulare.\n");
                trimite_tuturor("\n[!] Un jucator a iesit brusc. Runda se anuleaza.\n");
                joc_intrerupt = 1;
                break; 
            }

            char corect[5];
            strcpy(corect, set_intrebari[i].raspuns_corect);
            
            char raport[BUFF_SIZE];
            snprintf(raport, BUFF_SIZE, "\nTIMP EXPIRAT! Raspuns corect: %s\nClasament:\n", corect);

            pthread_mutex_lock(&mlock);
            for (int j = 0; j < max_clienti_index; j++) {
                if (!clienti[j].este_activ || strcmp(clienti[j].username, "Anonim") == 0) continue;

                if (strncasecmp(clienti[j].raspuns_curent, corect, 1) == 0) {
                    clienti[j].scor += 10;
                }
                char linie[100];
                snprintf(linie, 100, "%s: %d p\n", clienti[j].username, clienti[j].scor);
                strcat(raport, linie);
            }
            pthread_mutex_unlock(&mlock);

            strcat(raport, "\n----------------------\n");
            trimite_tuturor(raport);
            sleep(3);
        }

        if (joc_intrerupt) continue; 

        int max_scor = -1;
        char nume_castigatori[3500]; 
        bzero(nume_castigatori, 3500);
        int nr_castigatori = 0;

        pthread_mutex_lock(&mlock);
        for(int i=0; i<max_clienti_index; i++) {
            if (clienti[i].este_activ && strcmp(clienti[i].username, "Anonim") != 0) {
                if(clienti[i].scor > max_scor) max_scor = clienti[i].scor;
            }
        }
        for(int i=0; i<max_clienti_index; i++) {
            if (clienti[i].este_activ && strcmp(clienti[i].username, "Anonim") != 0) {
                if(clienti[i].scor == max_scor) {
                    if (nr_castigatori > 0) strcat(nume_castigatori, ", ");
                    strcat(nume_castigatori, clienti[i].username);
                    nr_castigatori++;
                }
            }
        }
        pthread_mutex_unlock(&mlock);

        bzero(buffer, BUFF_SIZE);
        if (max_scor <= 0) {
            snprintf(buffer, BUFF_SIZE, "\n>>> JOC TERMINAT! <<<\nNimeni nu a castigat (Scor: 0).\n");
        } else if (nr_castigatori > 1) {
            snprintf(buffer, BUFF_SIZE, "\n>>> JOC TERMINAT! <<<\nEGALITATE intre: %s (cu %d puncte)!\n", nume_castigatori, max_scor);
        } else {
            snprintf(buffer, BUFF_SIZE, "\n>>> JOC TERMINAT! <<<\nCASTIGATORUL ESTE: %s cu %d puncte!\n", nume_castigatori, max_scor);
        }
        trimite_tuturor(buffer);

        runda_curenta = -2; 
        
        pthread_mutex_lock(&mlock);
        for(int i=0; i<max_clienti_index; i++) {
            if(clienti[i].este_activ) bzero(clienti[i].raspuns_curent, 10);
        }
        pthread_mutex_unlock(&mlock);

        bzero(buffer, BUFF_SIZE);
        snprintf(buffer, BUFF_SIZE, "\n----------------------------------\nDORITI SA JUCATI DIN NOU? (Y/N)\nAveti 15 secunde sa decideti.\n----------------------------------\n");
        trimite_tuturor(buffer);

        printf("[GAME] Asteptam voturile (Y/N)...\n");

        for(int k=0; k<15; k++) {
            sleep(1);
            
            int toti_au_votat = 1;
            pthread_mutex_lock(&mlock);
            for(int i=0; i<max_clienti_index; i++) {
                if(clienti[i].este_activ && strcmp(clienti[i].username, "Anonim") != 0) {
                    if(strlen(clienti[i].raspuns_curent) == 0) {
                        toti_au_votat = 0; 
                        break; 
                    }
                }
            }
            pthread_mutex_unlock(&mlock);
            
            if(toti_au_votat) break; 
        }

        printf("[GAME] Procesam voturile...\n");
        
        pthread_mutex_lock(&mlock);
        for(int i=0; i<max_clienti_index; i++) {
            if(clienti[i].este_activ && strcmp(clienti[i].username, "Anonim") != 0) {
                
                char optiune = clienti[i].raspuns_curent[0];
                
                if(optiune == 'Y' || optiune == 'y') {
                    char msg[] = "Ati ales [DA]. Ramaneti in Lobby.\n";
                    write(clienti[i].socket, msg, strlen(msg));
                } 
                else {
                    char msg[] = "Ati ales [NU] sau nu ati raspuns. Deconectare...\n";
                    write(clienti[i].socket, msg, strlen(msg));
                    
                    close(clienti[i].socket);
                    clienti[i].este_activ = 0;
                    strcpy(clienti[i].username, "Anonim");
                    printf("[GAME] Jucatorul %d eliminat.\n", i);
                }
            }
        }
        pthread_mutex_unlock(&mlock);
        
    }
    return NULL;
}

void *thread(void *arg) {
    int id = *((int *) arg);
    int sock = clienti[id].socket;
    char buffer[BUFF_SIZE];
    int bytes;

    snprintf(buffer, BUFF_SIZE, "Bine ai venit! Te rog sa te autentifici: LOGIN <Nume>\n");
    write(sock, buffer, strlen(buffer));

    while (1) {
        bzero(buffer, BUFF_SIZE);
        bytes = read(sock, buffer, BUFF_SIZE - 1);

        if (bytes <= 0) {
            break;
        }

        while(bytes > 0 && (buffer[bytes-1] == '\n' || buffer[bytes-1] == '\r')) buffer[--bytes] = 0;

        printf("[Client ID %d] %s: %s\n", id, clienti[id].username, buffer);

        if (strncmp(buffer, "LOGIN", 5) == 0) {
            if (strlen(buffer) > 6) {
                pthread_mutex_lock(&mlock);
                strcpy(clienti[id].username, buffer + 6);
                clienti[id].scor = 0; 
                pthread_mutex_unlock(&mlock);

                char msg[100];
                snprintf(msg, 100, "ACK_LOGIN: Salut %s! Asteapta startul.\n", clienti[id].username);
                write(sock, msg, strlen(msg));
            }
        } 
        else if (strcmp(buffer, "QUIT") == 0) {
            printf("[Thread] Clientul %s a dat QUIT.\n", clienti[id].username);
            break;
        }
        else {
            if (joc_inceput && (runda_curenta >= 0 || runda_curenta == -2)) {
                pthread_mutex_lock(&mlock);
                strcpy(clienti[id].raspuns_curent, buffer);
                pthread_mutex_unlock(&mlock);
                
                if(runda_curenta == -2) 
                    write(sock, "Optiune inregistrata. Asteptam ceilalti jucatori...\n", 52);
                else 
                    write(sock, "Raspuns inregistrat.\n", 21);
            } else {
                write(sock, "Asteapta inceperea jocului...\n", 30);
            }
        }
    }

    close(sock);
    pthread_mutex_lock(&mlock);
    if(clienti[id].este_activ == 1) {
        clienti[id].este_activ = 0; 
        strcpy(clienti[id].username, "Anonim");
        printf("[Thread] Slotul %d a fost eliberat.\n", id);
    }
    pthread_mutex_unlock(&mlock);
    return NULL;
}

int main() {
    struct sockaddr_in server, from;
    int sd;

    incarca_intrebari_din_db("quiz.db");

    if ((sd = socket(AF_INET, SOCK_STREAM, 0)) == -1) { perror("Socket"); return errno; }
    int on = 1; setsockopt(sd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));

    bzero(&server, sizeof(server));
    server.sin_family = AF_INET;
    server.sin_addr.s_addr = htonl(INADDR_ANY);
    server.sin_port = htons(PORT);

    if (bind(sd, (struct sockaddr *) &server, sizeof(struct sockaddr)) == -1) { perror("Bind"); return errno; }
    listen(sd, 5);

    pthread_t game_th;
    pthread_create(&game_th, NULL, &game_logic, NULL);

    printf("Server pornit. Asteptam %d jucatori...\n", MIN_JUCATORI);

    while (1) {
        socklen_t len = sizeof(from);
        int client = accept(sd, (struct sockaddr *) &from, &len);

        pthread_mutex_lock(&mlock);
        
        int id = -1;
        for(int i=0; i<100; i++) {
            if(clienti[i].este_activ == 0) {
                id = i;
                break;
            }
        }
        
        if (id >= max_clienti_index) {
            max_clienti_index = id + 1;
        }

        clienti[id].socket = client;
        clienti[id].id_intern = id;
        clienti[id].este_activ = 1;
        clienti[id].scor = 0;
        strcpy(clienti[id].username, "Anonim");
        
        pthread_mutex_unlock(&mlock);

        pthread_t th;
        pthread_create(&th, NULL, &thread, &clienti[id].id_intern);
    }
    return 0;
}