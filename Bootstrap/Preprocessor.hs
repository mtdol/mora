module Preprocessor
(
  preprocess
)
where

preludeImport = Import "Prelude" (Excluding []) ""

--            File text  Include Prelude?
preprocess :: String ->  Bool
--      Output text  Module data   Import data
    -> (String,      ModuleP,      [ImportP])
preprocess text impPrelude = if impPrelude then
    (text,modAll,[preludeImport])
    else (text,modAll,[])




--
-- Types
--

type Label = String

data ModuleP = 
      Including [Label] 
    | Excluding [Label]

data ImportP = 
--         file  labels to include    "as" label
    Import Label ModuleP              Label

modAll = Excluding []


--
-- Parser
--

