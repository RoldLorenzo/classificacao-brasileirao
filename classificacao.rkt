#lang racket

;; --- DEFINIÇÃO DE ESTRUTURAS ---

;; Estrutura para representar um jogo.
;; anfitriao e visitante são os nomes dos times anfitriao e visitante.
;; gols-anfitriao e gols-visitante são a quantidade de gols dos respectivos times.

(struct Jogo (anfitriao gols-anfitriao visitante gols-visitante) #:transparent)
;; anfitriao, visitante : String
;; gols-anfitriao, gols-visitante : InteiroNãoNegativo

;; Estrutura para representar o desempenho de um certo time no campeonato.
;; Uma vitória aumenta pontos em 3.
;; Um empate aumenta pontos em 1.
;; vitorias representa a quantidade de vitórias do time.
;; saldo-gols representa o saldo de gols do time (gols feitos - gols sofridos).

(struct Desempenho (nome-time pontos vitorias saldo-gols) #:transparent)
;; nome-time : String
;; pontos, vitorias : InteiroNãoNegativo
;; saldo-gols : Inteiro

;; --- DECLARAÇÃO DE FUNÇÕES ---

;; ListaDeStrings Inteiro -> String
;;
;; Retorna a string na posicao indice de lista-de-str, se indice é menor que que o tamanho da lista e maior ou igual que 0,
;; retorna "" caso contrário.
;; É considerado que os elementos são indexados a partir do 1,
;; ou seja, o primeiro elemento corresponde ao índice 1, o segundo o 2 e assim por diante.

(define (acessa-indice lista-de-str indice)
  (cond
    [(empty? lista-de-str) ""] ;; Indice nao faz parte da lista
    [else
     (if (= indice 1)
         (first lista-de-str)
         (acessa-indice (rest lista-de-str) (sub1 indice)))]))

;; String -> Jogo
;;
;; Recebe uma linha da entrada e retorna jogo.
;; Requer que linha-entrada esteja no formato:
;; <nome-do-time-anfitrião> <gols-anfitrião> <nome-do-time-visitante> <gols-visitante>,
;; em que exista pelo menos um espaço entre os campos.

(define (string->jogo linha-entrada)
  (define campos (string-split linha-entrada))
  
  (define anfitriao      (acessa-indice campos 1))
  (define gols-anfitriao (string->number (acessa-indice campos 2)))
  (define visitante      (acessa-indice campos 3))
  (define gols-visitante (string->number (acessa-indice campos 4)))
  
  (Jogo anfitriao gols-anfitriao visitante gols-visitante))

;; String ListaDeStrings -> Bool
;;
;; Retorna #t caso str faça parte de lista-de-str,
;; retorna #f caso contrário.

(define (str-na-lista? lista-de-str str)
  (and
   (not (empty? lista-de-str))
   (or (equal? (first lista-de-str) str)
       (str-na-lista? (rest lista-de-str) str))))

;; Jogo ListaDeStr -> ListaDeStr
;;
;; Devolve lista-times junto com os times de jogo que ainda não estejam nela.
(define (insere-times-sem-repetir jogo lista-times)
  (define insere-anfitriao? (not (str-na-lista? lista-times (Jogo-anfitriao jogo))))
  (define insere-visitante? (not (str-na-lista? lista-times (Jogo-visitante jogo))))

  (cond
    [(and insere-anfitriao? insere-visitante?)
     (append (list (Jogo-anfitriao jogo) (Jogo-visitante jogo)) lista-times)]
    [insere-anfitriao? (cons (Jogo-anfitriao jogo) lista-times)]
    [insere-visitante? (cons (Jogo-visitante jogo) lista-times)]
    [else lista-times]))

;; ListaDeJogos -> ListaDeStrings
;;
;; Recebe lista-resultados e retorna uma lista com o nome de todos os times que participaram de algum jogo,
;; de modo que o nome de cada time apareça sem repetição.
(define (encontra-times lista-jogos)
  (foldr insere-times-sem-repetir empty lista-jogos))

;; Jogo -> String
;;
;; Recebe jogo e retorna:
;; O nome do time vencedor, caso exista,
;; "empate" caso contrário.

(define (resultado-jogo jogo)
  (cond
    [(> (Jogo-gols-anfitriao jogo) (Jogo-gols-visitante jogo)) (Jogo-anfitriao jogo)]
    [(> (Jogo-gols-visitante jogo) (Jogo-gols-anfitriao jogo)) (Jogo-visitante jogo)]
    [else "empate"]))

;; Desempenho Inteiro Inteiro Inteiro -> Desempenho
;;
;; Retorna uma copia de desempenho com pontos vitorias e saldo-de-gols adicionados aos seus respectivos campos

(define (adiciona-campos-desempenho desempenho pontos vitorias saldo-de-gols)
  (Desempenho (Desempenho-nome-time desempenho) (+ (Desempenho-pontos desempenho) pontos) (+ (Desempenho-vitorias desempenho) vitorias) (+ (Desempenho-saldo-gols desempenho) saldo-de-gols)))

;; Jogo Desempenho -> Desempenho
;;
;; Retorna uma copia de desempenho com os campos atualizados baseado no resultado de jogo.
;; Os campos serão atualizados da seguinte forma:
;; Caso o time de desempenho tenha ganhado: + 3 pontos e + 1 vitoria;
;; Caso tenha sido um empate: +1 ponto;
;; Além disso, deve ser adicionado o saldo de gols do time (gols feitos - gols sofridos) ao campo saldo-gols.
;; A função requer que o time de desempenho tenha participado do jogo, como anfitriao ou visitante.

(define (atualiza-desempenho jogo desempenho)
    (define resultado (resultado-jogo jogo))
    (define saldo-de-gols-vencedor (if (equal? resultado (Jogo-anfitriao jogo))
                                       (- (Jogo-gols-anfitriao jogo) (Jogo-gols-visitante jogo))
                                       (- (Jogo-gols-visitante jogo) (Jogo-gols-anfitriao jogo))))
  
    (cond
      [(equal? resultado (Desempenho-nome-time desempenho)) 
        (adiciona-campos-desempenho desempenho 3 1 saldo-de-gols-vencedor)]
      [(equal? resultado "empate")                          
        (adiciona-campos-desempenho desempenho 1 0 0)]
      [else                                                 
        (adiciona-campos-desempenho desempenho 0 0 (* -1 saldo-de-gols-vencedor))]))

;; String ListaDeJogos -> Desempenho
;;
;; Retorna o desempenho de um time a partir de nome-time e a lista de jogos.
;; Requer que na lista de jogos contenham APENAS jogos que o time jogou como anfitrião ou visitante.
(define (calcula-desempenho nome-time lista-jogos-time)
  (cond
    [(empty? lista-jogos-time) (Desempenho nome-time 0 0 0)]
    [else (atualiza-desempenho (first lista-jogos-time) (calcula-desempenho nome-time (rest lista-jogos-time)))]))

;; ListaDeStr ListaDeJogos -> ListaDeDeDesempenhos
;;
;; Retorna a lista de desempenhos dos times a partir de uma lista com o nome de cada time e a lista dos jogos.

(define (calcula-desempenhos lista-times lista-jogos)
  ;; Str -> ListaDeJogos
  ;; Usa lista-jogos
  ;;
  ;; Retorna uma lista dos jogos que o time tenha participado a partir de lista-jogos.
  (define (lista-jogos-time nome-time)
    (filter (λ (jogo)
              (or
               (equal? (Jogo-anfitriao jogo) nome-time)
               (equal? (Jogo-visitante jogo) nome-time)))
            lista-jogos))

  (map (λ (time) (calcula-desempenho time (lista-jogos-time time))) lista-times))

;; Desempenho ListaDeDesempenhos -> ListaDeDesempenhos
;;
;; Retorna uma copia de lista-desempenhos, com desempenho adicionado no lugar correto para que lista-desempenhos permaneça ordenada
;; de acordo com as regras de classificação.
;; Requer que lista-desempenhos já esteja ordenada.
;; A ordenacao é feita com o time vencedor, o segundo lugar em segundo ... o último lugar em último.
;; Os critérios para classificação de um time são:
;; Maior quantidade de pontos;
;; Maior quantidade de vitórias, caso exista empate de pontos;
;; Maior saldo de gols, caso exista empate nos outros critérios;
;; Ordem alfabética, caso exista empate nos outros critérios.

(define (insere-classificado desempenho lista-desempenhos)
  ;; Retorna #t caso desempenho1 ganhe de desempenho2 nos critérios especificados,
  ;; Retorna #f caso contrário.
  (define (classificacao-melhor? desempenho1 desempenho2)
    (or
     (> (Desempenho-pontos desempenho1) (Desempenho-pontos desempenho2))
     (and
      (= (Desempenho-pontos desempenho1) (Desempenho-pontos desempenho2))
      (> (Desempenho-vitorias desempenho1) (Desempenho-vitorias desempenho2)))
     (and
      (= (Desempenho-pontos desempenho1) (Desempenho-pontos desempenho2))
      (= (Desempenho-vitorias desempenho1) (Desempenho-vitorias desempenho2))
      (> (Desempenho-saldo-gols desempenho1) (Desempenho-saldo-gols desempenho2)))
     (and
      (= (Desempenho-pontos desempenho1) (Desempenho-pontos desempenho2))
      (= (Desempenho-vitorias desempenho1) (Desempenho-vitorias desempenho2))
      (= (Desempenho-saldo-gols desempenho1) (Desempenho-saldo-gols desempenho2))
      (string<? (Desempenho-nome-time desempenho1) (Desempenho-nome-time desempenho2)))))
  
  (cond
    [(empty? lista-desempenhos) (list desempenho)]
    [(classificacao-melhor? desempenho (first lista-desempenhos)) (cons desempenho lista-desempenhos)]
    [else (cons (first lista-desempenhos) (insere-classificado desempenho (rest lista-desempenhos)))]))

;; ListaDeDesempenhos -> ListaDeDesempenhos
;;
;; Retorna uma cópia de lista-desempenhos com os desemepenhos de cada time ordenados.
;; (De acordo com as regras de classificação já especificadas).

(define (classifica lista-desempenhos)
  (foldr insere-classificado empty lista-desempenhos))

;; Tipo Enumerado Direção é um dos valores:
;; - "esquerda"
;; - "direita"

;; String InteiroNãoNegativo Direção -> String
;;
;; Retorna uma string alinhada com base em str e tamanho
;; Caso o tamanho de str seja maior que tamanho retorna str.
;; Caso o tamanho de str seja menor que tamanho e a direção seja "esquerda" retorna uma string de tamanho caracteres onde os primeiros caracteres são str.
;; Caso o tamanho de str seja menor que tamanho e a direção seja "direita" retorna uma string de tamanho caracteres onde os ultimos caracteres são str.

(define (alinha-string str tamanho direcao)
  (cond
    [(>= (string-length str) tamanho) str]
    [(equal? direcao "esquerda") (string-append str (make-string (- tamanho (string-length str)) #\space))]
    [(equal? direcao "direita") (string-append (make-string (- tamanho (string-length str)) #\space) str)]))

;; Desempenho Inteiro -> String
;;
;; Retorna desempenho com os campos formatados em uma string baseado no tamanho do maior nome entre todos os times.
;; O tamanho-maior-nome-time será usado para alinhar o nome do time na string resultado.

(define (desempenho->string desempenho tamanho-maior-nome-time)  
  (string-append
   (alinha-string (Desempenho-nome-time desempenho) tamanho-maior-nome-time "esquerda")
   " "
   ;; Alinhando os numeros com strings de tamanho 5. Caso existam times com pontos maiores que 99.999, os números vão ficar desalinhados.
   (alinha-string (number->string (Desempenho-pontos desempenho)) 5 "direita")
   " "
   (alinha-string (number->string (Desempenho-vitorias desempenho)) 5 "direita")
   " "
   (alinha-string (number->string (Desempenho-saldo-gols desempenho)) 5 "direita")))

(define (classifica-times sjogos)
  (define lista-jogos (map string->jogo sjogos))

  (define times (encontra-times lista-jogos))

  (define desempenhos (calcula-desempenhos times lista-jogos))

  (define classificacao (classifica desempenhos))

  ;; Encontra o maior nome entre todos os times.
  (define tamanho-maior-nome-time (foldr (λ (f r) (max (string-length f) r)) 0 times))

  (map (curryr desempenho->string tamanho-maior-nome-time) classificacao))

(display-lines (classifica-times (port->lines)))