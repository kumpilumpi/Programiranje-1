
# def pomozna ( k, n ):
#     if n == 0:
#         return 1 + vsota
#     elif n < 0 :
#         return vsota
#     else:
#         # f(k, n -1 ) + f (k, n - 2) + ... + f (k, n - (k - 1 )) + f (k, n - k )  -> To bi žele zapisat 
#         for i in range (1, k + 1):
#             vsota = vsota + fun (k, n - 1)
#             return vsota


def fun ( k , n) :
    '''f (k, n) vrne št zaporedij N šz dolžine n, ki se začnejo z nič || raz med členoma manjša ali enaka k '''
    if n == 0:
        return 1 
    elif n < 0 :
        return 0
    else:
        # f(k, n -1 ) + f (k, n - 2) + ... + f (k, n - (k - 1 )) + f (k, n - k )  -> To bi žele zapisat 
        vsota = 0
        for i in range (1, k + 1):
            vsota = vsota + fun (k, n - i)
        return vsota   

print (fun (3,10))
    
    
    