
def pertenece(lista:[int],e:int):
   i:int=0
   while i < len(lista) and lista[i] != e :
      i+=1
      return i < len(lista)

def ordenados(lista:[int]) -> bool:
   i:int=0
   while i<len(lista)-1 and lista[i] <lista[i+1] :
      i+=1
      return i == len(lista)-1
   return False
print(ordenados([1,2,3,4]))

def es_palindromo(palabra:str):
   esPalindromo=True
   i=0
   for letra in palabra:
      if letra != palabra[len(palabra)-i-1]:
         esPalindromo=False
   return esPalindromo

print(es_palindromo("hh"),"es_palindromo")
##Ejercicio 7
def password_security2(contra:str):
   def hay_minuscula(contra):
      for letra in contra:
         if "A"<=letra<="Z":
            return True
      return False
   
   def hay_MAYUSCULA(contra):
         for letra in contra:
            if "a" <= contra<="z":
               return True
         return False
   def hay_Digito(contra):
      for letra in contra:
         if "0"<=contra<= "9":
            return True
      return False
   
   if len(contra)<5:
      return "Roja"
   if len(contra) >9:
      if hay_MAYUSCULA(contra) and hay_minuscula(contra) and hay_Digito(contra):
         return "Verde"
   return "Amarillo"


def movimientos_bancarios(movimientos:[(str,int)]):
   saldo=0
   for elem in movimientos:
      for operacion,cant_dinero in movimientos:
         if operacion == "R":
            saldo= saldo - cant_dinero
         if operacion == "I":
            saldo = saldo + cant_dinero
      return saldo

print(movimientos_bancarios([("I",2000),("R", 20),("R", 1000),("I", 300)]))


def long_mayor_a_7(lista:[str]):
   for palabra in lista:
      if len(palabra)> 7 :
         return True
   return False

"""
def long_mayor_a_7_2(lista:[str]):
   i:int=0
   while i <= len(lista) and :
      i+=1
      return False
   return True
"""
def hay_vocal(palabra):
      for letra in  range(0,len(palabra),1):
         if letra in "aeiou":
            return True
      return False 
def vocales_distintas(palabra:str)->bool:
   i:int=0
   hay_vocales=[]
   
   
   while  i < len(palabra) and hay_vocal(palabra) and not(palabra[i] in hay_vocales):
      hay_vocales.append(palabra[i])
      i+=1
      return True
   return False
   
         

print(vocales_distintas("holai"))


def pareas_a_0(lista:[int]):
   for elem in range(0,len(lista),1):
      if elem%2==0:
         lista[elem]=0


def nueva_lista_con_pares_a_0(lista:[int]):
   nueva_lista=[]
   for elem in range(0,len(lista),2):
      nueva_lista.append(lista[elem])
   return nueva_lista

#def sin_vocales(palabra):
