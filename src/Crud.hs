module Crud where

data Crud a b = Add a | AddMany [a] | Delete a | DeleteMany [a] | Get b

