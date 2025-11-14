module JogoDaForca (
    Estado(..), -- Exportando o tipo
    estadoInicial,
    printaPalavra,
    atualizaEstado,
    printaForca,
    checaVitoria,
    checaDerrota,
    letraFInal,
) where
import Data.String (IsString)

-- Definimos o estado do jogo
data Estado = State {
    palavra :: String,              --palavra selecionada
    letrasAdvinhadas :: String,     -- lista de letras já adivinhada pelo usuario
    erros :: Int                    -- quantidade de erros cometidos pelo usuário
} deriving (Show) -- 'deriving (Show)' nos permite imprimir o estado no GHCi para testes

-- Função com um estado de jogo inicial
estadoInicial :: String -> Estado
estadoInicial palavra = State {
    palavra = palavra,          -- Começamos com uma palabra
    letrasAdvinhadas = "",      -- Começamos sem nenhuma letra advinhada
    erros = 0                   -- Começamos com zero erros
}

-- printa a palavra na tela
printaPalavra :: Estado -> String
printaPalavra es = map letra (palavra es) -- Basicamente itera pelas letras da palavra
  where
    letra :: Char -> Char
    letra char = 
      if char `elem` (letrasAdvinhadas es) -- verifica se o char está na lista de chars encontrados
      then char
      else '_' -- senão permanece oculto

-- Tipo a função update em CG
atualizaEstado :: Char -> Estado -> Estado
atualizaEstado guess es
    | guess `elem` (letrasAdvinhadas es) = es -- Se já foi chutado
    | guess `elem` (palavra es) = es { letrasAdvinhadas = guess : (letrasAdvinhadas es) } -- Se estiver certo
    | otherwise = es { erros = (erros es) + 1, letrasAdvinhadas = guess : (letrasAdvinhadas es) } -- Se estiver errado incrementamos o número de erros

-- Função para o desenho da forca
printaForca :: Int -> String
printaForca e
    | e == 0 = 
        "     -------------------\n" ++
        "   /                   |\n" ++
        "  /                    |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  _____\n" ++
        " /     \\ \n" ++
        "|       |\n" ++
        "--------------------------------------------"
    | e == 1 =
        "     -------------------\n" ++
        "   /                   |\n" ++
        "  /                    |\n" ++
        "  |                    O\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  _____\n" ++
        " /     \\ \n" ++
        "|       |\n" ++
        "--------------------------------------------"
    | e == 2 =
        "     -------------------\n" ++
        "   /                   |\n" ++
        "  /                    |\n" ++
        "  |                    O\n" ++
        "  |                    |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  _____\n" ++
        " /     \\ \n" ++
        "|       |\n" ++
        "--------------------------------------------"
    | e == 3 =
        "     -------------------\n" ++
        "   /                   |\n" ++
        "  /                    |\n" ++
        "  |                    O\n" ++
        "  |                    |\\ \n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  _____\n" ++
        " /     \\ \n" ++
        "|       |\n" ++
        "--------------------------------------------"
    | e == 4 =
        "     -------------------\n" ++
        "   /                   |\n" ++
        "  /                    |\n" ++
        "  |                    O\n" ++
        "  |                   /|\\ \n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  _____\n" ++
        " /     \\ \n" ++
        "|       |\n" ++
        "--------------------------------------------"
    | e == 5 =
        "     -------------------\n" ++
        "   /                   |\n" ++
        "  /                    |\n" ++
        "  |                    O\n" ++
        "  |                   /|\\ \n" ++
        "  |                     \\ \n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  _____\n" ++
        " /     \\ \n" ++
        "|       |\n" ++
        "--------------------------------------------"
    | otherwise =
        "     -------------------\n" ++
        "   /                   |\n" ++
        "  /                    |\n" ++
        "  |                    O\n" ++
        "  |                   /|\\ \n" ++
        "  |                   /\\ \n" ++
        "  |\n" ++
        "  |\n" ++
        "  | \n" ++
        "  |\n" ++
        "  |\n" ++
        "  | \n" ++
        "  |\n" ++
        "  |\n" ++
        "  |\n" ++
        "  _____\n" ++
        " /     \\ \n" ++
        "|       |\n" ++
        "--------------------------------------------"

-- verifica se o jogador ganhou
checaVitoria :: Estado -> Bool
checaVitoria es = all (\c -> c `elem` (letrasAdvinhadas es)) (palavra es)

-- verifica se o jogador perdeu
checaDerrota :: Estado -> Bool
checaDerrota es =
    if (erros es >= 6) then
        True
    else
        False

-- verifica se estamos na letra final
letraFInal :: Estado -> Bool
letraFInal es =
    let revealed = filter (\c -> c `elem` (letrasAdvinhadas es)) (palavra es)
    in length revealed == (length (palavra es) - 1)