import math
import random

ldots = "..."

def f1():
    a = random.randint(1, 99)
    b = random.randint(1, 99)
    c = a + b

    tup = map( lambda x: str(x), [a, b, c] )
    tup[ random.randint( 0, len( tup ) - 1 ) ] = ldots
    
    return "%s + %s = %s" % tuple( tup )

def f2():
    a = random.randint(2, 99)
    b = random.randint(2, 9)
    c = a * b

    while c > 100:

        a = random.randint(2, 99)
        b = random.randint(2, 9)
        c = a * b

    tup = map( lambda x: str(x), [a, b, c] )
    tup[ random.randint( 0, len( tup ) - 1 ) ] = ldots
    
    return "%s x %s = %s" % tuple( tup ) 

def f3():
    a = random.randint(1, 99)
    b = random.randint(1, 99)
    c = random.randint(1, 9)
    d = a + b + c

    tup = map( lambda x: str(x), [a, b, c, d] )
    tup[ random.randint( 0, len( tup ) - 1 ) ] = ldots
    
    return "%s + %s + %s = %s" % tuple( tup )


def f4():
    a = random.randint(1, 99)
    b = random.randint(1, a)
    c = a - b

    tup = map( lambda x: str(x), [a, b, c] )
    tup[ random.randint( 0, len( tup ) - 1 ) ] = ldots
    
    return "%s - %s = %s" % tuple( tup ) 

def f5():
    a = random.randint(1, 99)
    b = random.randint(1, min(a, 9))

    tup = map( lambda x: str(x), [a, b] )
    
    return "%s : %s = %s reste %s" % tuple( tup + [ ldots, ldots ] )

def f6():
    a = float(random.randint(1, 999)) / (10.0 ** random.randint(1, 2))
    b = float(random.randint(1, 999)) / (10.0 ** random.randint(1, 2))
    c = a + b

    tup = map( (lambda x: "%g" % x), [a, b, c] )
    tup[ random.randint( 0, len( tup ) - 1 ) ] = ldots
    
    return "%s + %s = %s" % tuple( tup ) 

def f7():
    a = 0.0
    b = 0.0
    while b > a or b < 1.0:
        a = float(random.randint(1, 300)) / (10.0 ** random.randint(1, 3) )  + 1.0
        b = float(random.randint(0, 100)) / (10.0 ** random.randint(1, 2))
        #print a, b, b > a, b < 1.0
    c = a - b

    tup = map( (lambda x: "%g" % x), [a, b, c] )
    tup[ random.randint( 0, len( tup ) - 1 ) ] = ldots
    
    return "%s - %s = %s" % tuple( tup ) 


def f8():
    a = float(random.randint(1, 99)) / (10.0 ** random.randint(1, 2))
    b = 10 ** random.randint(1, 3)
    c = a * b

    tup = [ "%g" % a, "%d" % b, "%g" % c]
    tup[ random.randint( 0, len( tup ) - 1 ) ] = ldots
    
    return "%s x %s = %s" % tuple( tup ) 

l = [ f1, f2, f3, f4, f5, f6, f7, f8 ]


if True:
    nb_page = 100
    
    txt = [ [ random.choice( l )() for x in range( 0, 32 * nb_page )  ] for y in range( 0, 3 ) ]

    #######
 
    for i in range( 0, len( txt ) ):
        
        l_max = max( [ len( x ) for x in txt[ i ] ] )
        
        for j in range( 0, len( txt[ i ] ) ):
            
            txt[i][j] = txt[i][j] + " " * ( l_max - len( txt[i][j] ) )


    f = open( "exercices.txt", "w")        
    for j in range( 0, len( txt[ 0 ] ) ):
        f.write( "     ".join(txt[i][j] for i in range( 0, len( txt ) )) + "\n\n" )
        f.flush()
    f.close()    
else:
    l = [ f2 ]
    
    for x in l:
        print x()
print "done"
