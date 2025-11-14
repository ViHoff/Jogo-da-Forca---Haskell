import JogoDaForca --importa módulo
import System.IO (readFile) --ler arquivo
import System.Random (randomRIO) --numeros aleatorios
import Data.Char (toUpper) --manipulacao de caracteres para transformar em maiusculo o palpite



sorteioDePalavra :: IO String
sorteioDePalavra = do
    --primeiro vai ler o conteudo dentro do arquivo .txt
    conteudoDic <- readFile "dicionario.txt"
    
    let palavras = lines conteudoDic --apos isso, transforma o que tinha dentro do arquivo em uma lista de palavras
    let maxIndice = length palavras - 1
    indice <- randomRIO (0, maxIndice) --sorteia um indice aleatorio
    --o return IO embrulha um valor puro em um valor IO
    return (palavras !! indice) --vai retornar a palavra sorteada no indice randomizado
clean :: IO ()
clean = putStr (replicate 50 '\n') --função feita para limpar a tela
--funcao principal
--é uma funcao recursiva que vai chamar ela mesma até o jogo acabar - loop


gameLoop :: Estado -> IO ()
gameLoop estado = do
    clean
    putStrLn ("|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||") 
    putStrLn (printaForca (erros estado)) --desenha a forca
    putStrLn ("")  -- cria uma linha em branco
    putStrLn (printaPalavra estado) --mostra a palavra com as letras ja advinhadas
    putStrLn ("")  
    putStrLn ("Letras já utilizadas: " ++ letrasAdvinhadas estado) --mostra as letras ja advinhadas
    putStrLn ("Número de erros: " ++ show (6 - erros estado)) --mostra o numero de erros
    putStrLn ("|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||") 

    --vai verificar se o usuario ganhou ou perdeu o jogo
    if checaVitoria estado then do
        putStrLn "Uhulllll, você acertou!!!!"
        return ()

    else if checaDerrota estado then do
        putStrLn ("Que pena! Você foi derrotado pelo haskell! A palavra era: " ++ palavra estado)
        return ()

    else do
    -- Regra para a chance de troca de palavra
        if (letraFInal estado)
        then do
            chance <- randomRIO (1,4 :: Int)
            if (chance == 1)
            then do
                putStrLn ("OPAAA!! ALGO OCORREU!!!!! O jogo mudou a palavra!!")
                nova <- sorteioDePalavra
                let novaMaiuscula = map toUpper nova
                let novoEstado = estado { palavra = novaMaiuscula }
                gameLoop novoEstado -- reinicia com a palavra nova recem sorteada
                putStrLn ("OPAAA!! ALGO OCORREU!!!!! O jogo mudou a palavra!!")
            else do
                pedeChute estado -- palavra nova nao sorteada, pede chute normalmente
        else do
            pedeChute estado -- executa quando nao estamos na letra final
        

        
pedeChute :: Estado -> IO ()
pedeChute estado = do
    putStrLn "Escolha uma letra para advinhar: "
    chute <- getLine
    let try =  if null chute then ' ' else head chute --serve para pegar o primeiro caractere
    let chuteMaiusculo = toUpper try --transforma o chute em maiusculo

    if chuteMaiusculo `elem` (letrasAdvinhadas estado)
    then do
        putStrLn "Você já tentou essa letra antes! Tente outra."
        putStrLn "Aperte enter para continuar..."
        _ <- getLine
        gameLoop estado -- volta para o loop principal sem atualizar o estado
    else do
        let novoEstado = atualizaEstado chuteMaiusculo estado --atualiza o estado com o chute
        gameLoop novoEstado --volta para o loop principal com o estado atualizado


runGame :: IO ()
runGame = do
    palavraSorteada <- sorteioDePalavra --sorteia a palavra
    let palavraMaiuscula = map toUpper palavraSorteada
    let estadoInicialDoJogo = estadoInicial palavraMaiuscula 

    gameLoop estadoInicialDoJogo

    putStr "Você deseja se desafiar e jogar novamente??? xD (s/n): " 
    linha <- getLine

    --Poe em maiusculo a resposta do usuario
    let resposta = if null linha then ' ' else toUpper (head linha)

     --verifica se o usuario quer jogar novamente

    if resposta == 'S'
    then runGame --chama a funcao novamente
    else putStrLn "Você encerrou sua jornada na forca do Haskell..."





main :: IO ()
main = do
    putStrLn "Bem vindo ao Jogo da Forca em Haskell!"
    runGame


