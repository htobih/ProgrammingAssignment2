## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Diese Funktion erstellt ein spezielles "Matrix"-Objekt, das in der Lage ist,
# seine Inverse zu speichern, falls benötigt. 
makeCacheMatrix <- function(x = matrix()) {
        # Initialisiere einen Platzhalter für die gespeicherte Inverse
        m <- NULL
        
        # Definiere eine Funktion, um den Wert der Matrix zu setzen
        set <- function(y) {
                x <<- y         # Speichere die neue Matrix
                m <<- NULL      # Setze die gespeicherte Inverse zurück, da sie nicht mehr gültig ist
        }
        
        # Definiere eine Funktion, um den Wert der Matrix zu erhalten
        get <- function() x
        
        # Definiere eine Funktion, um die Inverse im Cache zu speichern
        setinverse <- function(inverse) m <<- inverse
        
        # Definiere eine Funktion, um die gespeicherte Inverse abzurufen
        getinverse <- function() m
        
        # Gebe eine Liste dieser Funktionen zurück, um mit der Matrix zu interagieren
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#### cacheSolve Funktion
# Diese Funktion berechnet die Inverse der Matrix, die vom 'makeCacheMatrix' Objekt zurückgegeben wird.
# Wenn die Inverse bereits berechnet und im Cache gespeichert wurde, verwendet sie die gespeicherte Inverse,
# um unnötige Berechnungen zu vermeiden.
cacheSolve <- function(x) {
        # Versuche, die gespeicherte Inverse abzurufen
        m <- x$getinverse()
        
        # Überprüfe, ob die Inverse bereits im Cache vorhanden ist
        if(!is.null(m)) {
                message("getting cached data")  # Benachrichtige den Benutzer, dass die Inverse aus dem Cache kommt
                return(m)                       # Gebe die gespeicherte Inverse zurück
        }
        
        # Falls die Inverse nicht im Cache ist, berechne sie
        data <- x$get()                   # Hole die aktuelle Matrix
        m <- solve(data)                  # Berechne die Inverse der Matrix
        
        # Speichere die berechnete Inverse im Cache für zukünftige Verwendungen
        x$setinverse(m)
        
        # Gebe die neu berechnete Inverse zurück
        m
}

        ## Return a matrix that is the inverse of 'x'

# Testmatrix 1
matrix1 <- makeCacheMatrix(matrix(c(2, 3, 1, 4), nrow = 2, ncol = 2))
# Berechne und cache die Inverse von matrix1
inverse1 <- cacheSolve(matrix1)
print(inverse1)
# Versuche, die Inverse von matrix1 erneut abzurufen, um zu prüfen, ob sie aus dem Cache kommt
