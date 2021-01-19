# A

def postavitev (dol, kor, stevilo, ni_kor = True): #ni kor je bool, ki bo True ->je bilo postavljeno korito -> vemo da naslednje ne bo korito
    if dol < (stevilo * kor + stevilo - 1): #Takrat ne bo možne postavitve za korita
        return 0
    elif dol == 0 :
        return 1
    else:
        #da se korita ne dotikajo
        if ni_kor :
            return postavitev (dol - 1, kor, stevilo, True) + postavitev (dol - kor, kor, stevilo -1, False) #lahko damo korito ali prazno 
        else:
            return postavitev (dol - 1, kor, stevilo, True) #ker je bilo prej postavljeno korito lahko damo le prazno

# B

def postavitev_raz (n, sez) = True
