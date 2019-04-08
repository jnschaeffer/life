module Lib
    ( someFunc
    , NodeType
    , Node
    ) where

data NodeType = Add
              | Subtract
              | EqZero
              | LTZero
              | GTZero
              | Number Integer
              deriving (Show)

data Node = Node NodeType [Node]
          deriving (Show)

evalBranchNode :: (Integer -> Bool) -> Node -> Either String Integer
evalBranchNode f (Node _ (condNode:thenNode:elseNode:_)) =
  let chooseNode v
        | (f v)     = thenNode
        | otherwise = elseNode
  in (evalNode condNode) >>= (evalNode . chooseNode)
evalBranchNode _ _ = Left "invalid input on branch node"

evalBinaryNode :: (Integer -> Integer -> Either String Integer) -> Node -> Either String Integer
evalBinaryNode f (Node _ (leftNode:rightNode:_)) = do
  leftResult  <- evalNode leftNode
  rightResult <- evalNode rightNode
  f leftResult rightResult
evalBinaryNode _ _ = Left "invalid input on binary node"

evalNode :: Node -> Either String Integer
evalNode n@(Node EqZero _) =
  evalBranchNode ((==) 0) n
evalNode n@(Node LTZero _) =
  evalBranchNode ((<) 0) n
evalNode n@(Node GTZero _) =
  evalBranchNode ((>) 0) n
evalNode n@(Node Add _) =
  evalBinaryNode (\x y -> Right (x + y)) n
evalNode n@(Node Subtract _) =
  evalBinaryNode (\x y -> Right (x - y)) n
evalNode (Node (Number x) _) = Right x
          

someFunc :: IO ()
someFunc = putStrLn <$> show $ evalNode (Node EqZero [
                                            (Node Subtract [
                                                (Node (Number 10) []),
                                                (Node (Number 9) [])
                                                ]),
                                            (Node (Number 1) []),
                                            (Node (Number 2) [])
                                            ])
