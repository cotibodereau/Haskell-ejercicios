data EmpresaTelefono = Claro | Personal | Movistar | Tuenti deriving (Eq, Show)
type Frase = String

fraseEmpresa :: EmpresaTelefono -> Frase
fraseEmpresa Claro = "Claro, La red mas poderosa"
fraseEmpresa Personal = "Personal, es como vos"
fraseEmpresa Movistar = "Movistar, Compartida la vida es mas linda"
fraseEmpresa Tuenti = "Tuenti es la mas economica"

type NombrePersona = String
data MisEmpresas = AgregaEmpresa EmpresaTelefono NombrePersona MisEmpresas | Ninguna 

tengoEmpresa :: MisEmpresas -> EmpresaTelefono -> NombrePersona -> Bool
tengoEmpresa Ninguna _ _ = False
tengoEmpresa (AgregaEmpresa t1 n1 m1) t2 n2 | (t1 == t2 && n1 == n2)= True
                                            | otherwise = False
                                            
type NroTel = Int
type ListaAsoc a b = [(a, b)]

agregaLA :: ListaAsoc EmpresaTelefono NroTel -> EmpresaTelefono -> NroTel -> ListaAsoc EmpresaTelefono NroTel
agregaLA  listaAsoc t1 n1 = (t1, n1) : listaAsoc

{- *Main> agregaLA (agregaLA [] Personal 4545) Personal 629962
[(Personal,629962),(Personal,4545)]
*Main> agregaLA [] Claro 3517696824
[(Claro,3517696824)] -}


data Arbol a = Hoja a | Nodo (Arbol a) a (Arbol a) deriving (Show, Eq)
aBusca :: Eq a => Arbol a -> a -> Bool
aBusca (Hoja v) x = v == x
aBusca (Nodo izq v der) x = aBusca izq x || v == x || aBusca der x

{- *Main> aBusca (Nodo (Hoja  3) 3 (Hoja  5)) 3
True
*Main> aBusca (Nodo (Hoja  3) 3 (Hoja  5)) 4
False
*Main> aBusca (Nodo (Hoja  3) 3 (Hoja  5)) 5
True
*Main> aBusca (Nodo (Hoja  3) 3 (Hoja  5)) 6
False -}









