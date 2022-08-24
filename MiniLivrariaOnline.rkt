#lang scheme
(define (lê-string prompt)
  (display prompt)
  (read-line))

(define (lê-número prompt)
  (string->number (lê-string prompt)))

(define (main-loop)
  (cond [(menu) (main-loop)]
        [else (void)]))


(define (menu)
  (display "    === Menu ===
  (1) Inserir Livro
  (2) Inserir Cliente  
  (3) Listar todos os Livros
  (4) Listar todos os cliente
  (5) Procurar Livro por nome
  (6) Remover livro
  (7) Comprar Livros
  (8) Listar compras
  (0) Sair\n")
  (case (lê-número "Digite uma opção: ")
    [(1) (inserir-livro) #t]
    [(2) (inserir-cliente)#t]
    [(3) (listar-livros)#t]
    [(4) (listar-clientes) #t]
    [(5) (procurar-livro) #t]
    [(6) (remover-livro) #t]
    [(7) (realizar-compraL) #t]
    [(8) (listar-compras) #t]
    [(0) #f]
    [else (display "Opção inválida!\n") #t]))

;;lista de livros.
(define livrosList '())

;;lista de clientes
(define clientesList '())

;;relacionamento cliente-livro
(define compraList '())

;; Funções que abstraem a criação e obtenção dos campos de um livro.
  
  (define (novo-livro id nome ISBN autor editor ano) (list id nome ISBN autor editor ano))
  (define (id-do-livro Livro) (car Livro))
  (define (nome-do-livro Livro) (cadr Livro))
  (define (ISBN-do-livro Livro) (caddr Livro ))
  (define (autor-do-livro Livro) (cadddr Livro))
  (define (editor-do-livro Livro) (cadddr(cdr Livro)))
  (define (ano-do-livro Livro) (cadddr(cdr (cdr Livro))))

;; Função que abstraem a criação e a obtenção dos campos de um cliente.
  (define (novo-cliente id nome apelido nacionalidade email login password cartao profissao endereco telefone) (list id nome apelido nacionalidade email login password cartao profissao endereco telefone))
  (define (id-do-cliente Cliente) (car Cliente))
  (define (nome-do-cliente Cliente) (cadr Cliente))
  (define (apelido-cliente Cliente) (caddr Cliente ))
  (define (nacionalidade-cliente Cliente) (cadddr Cliente))
  (define (email-do-cliente Cliente) (cadddr(cdr Cliente)))
  (define (login-do-cliente Cliente) (cadddr(cdr (cdr Cliente))))
  (define (password Cliente) (cadddr(cdr (cddr Cliente))))
  (define (numero-do-cartao Cliente) (cadddr(cdr (cdddr Cliente))))
  (define (profissao Cliente) (cadddr(cdr (cddddr Cliente))))
  (define (endereco Cliente) (cadddr(cddr (cddddr Cliente))))
  (define (telefone Cliente) (cadddr(cdddr (cddddr Cliente))))




;; Função que abstraem a criação e a obtenção dos campos de um compra.    
  (define (nova-compra nome-cliente nome-livro) (list nome-cliente nome-livro))
  (define (livro-c compra) (car compra))
  (define (cliente-c compra) (cadr compra))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;inserção;;;;;;;;;;;;;;;;;;;;;;;;;



;;; Inserção de livro.
  (define (inserir-livro)
  (define id (lê-string "ID: "))
  (define nome (lê-string "Nome: "))
  (define ISBN (lê-string "ISBN: "))
  (define autor (lê-string "Autor(es): "))
  (define editor (lê-string "Editor(es): "))
  (define ano (lê-string "ano da publicação: "))
  (set! livrosList (cons (novo-livro id nome ISBN autor editor ano) livrosList))  
  (printf "\n livro inserido com sucesso! \n \n"))

;;;; Inserção cliente
  (define (inserir-cliente)
  (define id (lê-string "ID: "))
  (define nome (lê-string "Nome: "))
  (define apelido(lê-string "Apelido: "))
  (define nacionalidade (lê-string "nacionalidade: "))    
  (define email (lê-string "Email: "))
  (define login (lê-string "login: "))    
  (define password (lê-string "Password: "))
  (define cartao (lê-string "Numero do cartão: "))
  (define profissao(lê-string "Profissao: "))
  (define endereco (lê-string "Endereço: "))
  (define telefone (lê-string "Telefone: "))
  (set! clientesList (cons (novo-cliente id nome apelido nacionalidade email login password cartao profissao endereco telefone) clientesList))
  (printf "\n cliente inserido com sucesso! \n \n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Listagem.;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1. Função que recebe _um_ Livro e imprime:
(define (imprime-livro Livro)
  (printf "\n =====Exibir Livros==== \n")
  (printf "ID: ~a\n" (id-do-livro Livro))
  (printf "Nome: ~a\n" (nome-do-livro Livro))
  (printf "ISBN: ~a\n" (ISBN-do-livro Livro))
  (printf "Autores: ~a\n" (autor-do-livro Livro))
  (printf "Editores: ~a\n" (editor-do-livro Livro))
  (printf "Ano da publicação: ~a\n" (ano-do-livro Livro))
  (printf "\n"))

;;1.1 Função que recebe um cliente e imprime:
(define (imprime-cliente Cliente)
  (printf "\n =====Exibir clientes==== \n")
  (printf "ID: ~a\n" (id-do-cliente Cliente))
  (printf "Nome: ~a\n" (nome-do-cliente Cliente))
  (printf "Apelido: ~a\n" (apelido-cliente Cliente) )
  (printf "Nacionalidade: ~a\n" (nacionalidade-cliente Cliente))  
  (printf "Email: ~a\n" (email-do-cliente Cliente))
  (printf "login: ~a\n" (login-do-cliente Cliente))  
  (printf "Password: ~a\n" (password Cliente))
  (printf "Cartão: ~a\n" (numero-do-cartao Cliente))
  (printf "Profissao: ~a\n" (profissao Cliente))
  (printf "Endereço: ~a\n" (endereco Cliente))
  (printf "Telefone: ~a\n" (telefone Cliente))
  (printf "\n"))

(define (imprime-livro-compra Livro)
  (printf "ID: ~a\n" (id-do-livro Livro))
  (printf "Nome: ~a\n" (nome-do-livro Livro))
  (printf "ISBN: ~a\n" (ISBN-do-livro Livro))
  (printf "Autores: ~a\n" (autor-do-livro Livro))
  (printf "Editores: ~a\n" (editor-do-livro Livro))
  (printf "Ano da publicação: ~a\n" (ano-do-livro Livro))
  (printf "\n"))

;;1.1 Função que recebe um cliente e imprime:
(define (imprime-cliente-compra Cliente)
  (printf "ID: ~a\n" (id-do-cliente Cliente))
  (printf "Nome: ~a\n" (nome-do-cliente Cliente))
  (printf "Apelido: ~a\n" (apelido-cliente Cliente) )
  (printf "Nacionalidade: ~a\n" (nacionalidade-cliente Cliente))  
  (printf "Email: ~a\n" (email-do-cliente Cliente))
  (printf "login: ~a\n" (login-do-cliente Cliente))  
  (printf "Password: ~a\n" (password Cliente))
  (printf "Cartão: ~a\n" (numero-do-cartao Cliente))
  (printf "Profissao: ~a\n" (profissao Cliente))
  (printf "Endereço: ~a\n" (endereco Cliente))
  (printf "Telefone: ~a\n" (telefone Cliente))
  (printf "\n"))


 
;; Função que recebe uma lista de livrosList e imprime (o loop):
(define (imprime-livrosList lst)
  (cond
   ;; Se chegamos ao fim da lista (i.e., à lista vazia), nada a fazer.
   [(null? lst) (printf "\n Sem Livros cadastrados! \n\n")(void)]
   ;; se tiver apenas um elemento.
   [(= (comprimento lst) 1)
      (imprime-livro (car lst))]
   ;; se tiver mais de um elemento.
   [else (imprime-livro (car lst))
        (imprime-livrosList (cdr lst)) ] ) )

;;2.1 recebe uma lista clienteList e imprime
(define (imprime-clientesList lst)
  (cond
   ;; Se chegamos ao fim da lista (i.e., à lista vazia), nada a fazer.
   [(null? lst) (printf "\n Sem clientes cadastrados! \n\n")(void)]
   ;; se tiver apenas um elemento.
   [(= (comprimento lst) 1)
      (imprime-cliente (car lst))]
   ;; se tiver mais de um elemento.
   [else (imprime-cliente (car lst))
        (imprime-clientesList (cdr lst)) ] ) )



;; 3. Finalmente, função que imprime a lista global de livrosList (chamada a partir do menu).
(define (listar-livros)
  (imprime-livrosList livrosList))

;; 3.1  função que imprime a lista global de clientesList (chamada a partir do menu)
(define (listar-clientes)
  (imprime-clientesList clientesList))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Procura por nome.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;procurar livro
(define (procura-livro nome livrosList)
  (cond
   ;; Se chegamos na lista vazia, é porque não achamos nenhum
   ;; livro com o nome procurado.  Nesse caso, retornaremos #f.
   [(null? livrosList) #f]
   ;; Caso contrário, olhamos para o primeiro elemento da lista.
   ;; Se ele tiver o nome que estamos procurando, retornamos o livro.
   [(string=? nome (nome-do-livro (car livrosList)))  (car livrosList)]
   ;; Caso contrário, procuramos no restante da lista.
   [else (procura-livro nome (cdr livrosList))]))


;procurar cliente
(define (procura-cliente nome clientesList)
  (cond
   ;; Se chegamos na lista vazia, é porque não achamos nenhum
   ;; cliente com o nome procurado.  Nesse caso, retornaremos #f.
   [(null? clientesList) #f]
   ;; Caso contrário, olhamos para o primeiro elemento da lista.
   ;; Se ele tiver o nome que estamos procurando, retornamos o cliente.
   [(string=? nome (nome-do-cliente (car clientesList)))  (car clientesList)]
   ;; Caso contrário, procuramos no restante da lista.
   [else (procura-cliente nome (cdr clientesList))]))



(define (procurar-livro);modifiqued
  (define nome (lê-string "Nome procurado: "))
  (define Livro (procura-livro nome livrosList))
  (cond
   [Livro (imprime-livro Livro)]
   [else (display "Livro não encontrado!\n")]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Remoção.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (remove-livro nome livrosList)
  (cond
   ;; Se a lista é vazia, não há livrosList a remover.
   [(null? livrosList) livrosList (printf "\n livro não encontrado! \n\n")]
   ;; Se o nome do primeiro da lista é igual ao nome a ser removido,
   ;; devolvemos a lista sem o primeiro elemento.
   [(string=? nome (nome-do-livro (car livrosList)))  (cdr livrosList) (printf "\n livro removido com sucesso! \n\n") ]
   ;; Se o primeiro da lista não é o elemento a ser removido, ele vai
   ;; continuar sendo o primeiro elemento da lista resultado. Já o
   ;; restante do resultado vai ser o restante da lista original,
   ;; _depois de aplicada a rotina de remoção_.
   [else (cons (car livrosList)
               (remove-livro nome (cdr livrosList)))]))



(define (remove-cliente nome clientesList)
  (cond
   ;; Se a lista é vazia, não há livrosList a remover.
   [(null? clientesList) clientesList (printf "\n cliente não encontrado! \n\n")]
   ;; Se o nome do primeiro da lista é igual ao nome a ser removido,
   ;; devolvemos a lista sem o primeiro elemento.
   [(string=? nome (nome-do-cliente (car clientesList)))  (cdr clientesList) (printf "\n cliente removido com sucesso! \n\n") ]
   ;; Se o primeiro da lista não é o elemento a ser removido, ele vai
   ;; continuar sendo o primeiro elemento da lista resultado. Já o
   ;; restante do resultado vai ser o restante da lista original,
   ;; _depois de aplicada a rotina de remoção_.
   [else (cons (car clientesList)
               (remove-cliente nome (cdr clientesList)))]))





(define (remove-compra nome compraList)
  (cond
   ;; Se a lista é vazia, não há livrosList a remover.
   [(null? (car compraList)) compraList ]
   ;; Se o nome do primeiro da lista é igual ao nome a ser removido,
   ;; devolvemos a lista sem o primeiro elemento.
   [(string=? nome (nome-do-livro (livro-c (car compraList))))  (cdr compraList)(printf "\n compra removida com sucesso!\n\n") ]
   ;; Se o primeiro da lista não é o elemento a ser removido, ele vai
   ;; continuar sendo o primeiro elemento da lista resultado. Já o
   ;; restante do resultado vai ser o restante da lista original,
   ;; _depois de aplicada a rotina de remoção_.
   [else (cons (car compraList)
               (remove-compra nome (cdr compraList)))]))
   
(define (remover-livro)
  (define nome (lê-string "Nome a remover: "))
  (set! livrosList (remove-livro nome livrosList)))
  ;(set! compraList (cons (remove-compra nome ))))
;(define (remover-cliente);modifiqued
  ;(define nome (lê-string "Nome a remover: "))
  ;(set! clientesList (remove-cliente nome clientesList))
  ;(set! compraList (remove-cliente nome clientesList)))



;;;;;;;;;;;;;;;;;;;;;;;;INSERÇÃO E LISTAGEM DE COMPRAS;;;;;;;;;;;;;;;



;;;;;;;;; Inserção de Compras.;;;;;;;;;;;;

  (define (realizar-compraL)
  (define nomeL (lê-string "Nome do livro: "))
  (define Livro (procura-livro nomeL livrosList))
  (cond
      [Livro(realiza-compraC Livro)]           
        [else (display "Livro não encontrado!\n")]))
  
  (define (realiza-compraC Livro)
  (define nomeC (lê-string "Nome do cliente: "))
  (define Cliente(procura-cliente nomeC clientesList))
  (cond
     [Cliente(compra-salva Livro Cliente)]
        [else (display "cliente não encontrado!\n")]))

 (define (compra-salva Livro Cliente)      
      (set! compraList (cons (nova-compra Livro Cliente) compraList))
      (printf "\n Compra finalizada com sucesso! \n\n"))

    
 

;;;;;;;;;;;;;listagem compras;;;;;;;;;

;; 1. Função que recebe _uma_ compra e imprime:
(define (imprime-compra compra)
  (printf "\n =====Exibir Compras==== \n")
  (printf "\nDados do cliente: \n")
  (imprime-cliente-compra (cliente-c compra))
  (printf "livro comprado: \n" )
  (imprime-livro-compra (livro-c compra))
  (printf "\n"))

;; 2. Função que recebe uma lista de compras e imprime (o loop):
(define (imprime-compraList lst)
  (cond
   ;; Se chegamos ao fim da lista (i.e., à lista vazia), nada a fazer.
   [(null? lst) (void)]
   ;; Senão, imprimimos a primeiro compra e repetimos a função para o restante da lista.
   [else (imprime-compra (car lst))
         (imprime-compraList (cdr lst))]))

;; 3. Finalmente, função que imprime a lista global de compras (chamada a partir do menu).
(define (listar-compras)
  (imprime-compraList compraList))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;funcão para ver tamanho
(define comprimento (lambda (lista) (if (null? lista) 0 (+ 1
 (comprimento (cdr lista))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(main-loop)