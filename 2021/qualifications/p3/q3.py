from math import sqrt
def get_cumulative_tab (l, n):
    tab = n*[0]
    s = 0
    for i in range(n):
        s += l[i]
        tab[i] = s
    return tab

def tab_somme_elts(tab, cum_tab, n):
    '''tableau dont les élement sont les sommes des sous-listes'''
    tab_sum = []
def get_sommes(cum_tab, x):
    '''Donne les deux sommes solution ou renvoit -1'''
    # -> on coupe en deux la liste cum_tab donnant l1 et l2, une avec les élements > sqrt(x), l'autre avec le reste
    # on cherche d'abord l'indice auquel on veut séparer les listes
    i_sep = len(cum_tab)             # cas où tous les élements de la liste sont inf à sqrt(x), la 2e liste et vide et on ne poura pas obtenir x
    for i in range(len(cum_tab)):
        if cum_tab[i] > int(sqrt(x)) and i<i_sep:
            i_sep = i

    l1 = cum_tab[:i_sep]
    l2 = cum_tab[i_sep:]
    print(cum_tab)
    print(i_sep)
    print(l1)
    print(l2)
    for s2 in l2:
        for s1 in l1:
            print(f"s1:{s1} s2:{s2} produit:{s1*s2}")
            if s1*s2 == x:
                return [s1,s2] # s1 < s2
    return -1

def sousliste_of_somme(l, cum_tab, s, n):
    best_list = []
    best_len = 0
    for pos1 in range(n):
        for pos2 in range(n):
            if cum_tab[pos2] - cum_tab[pos1] -1 == s:
                if pos2 - pos1 > best_len:
                    best_list = l[pos1:pos2+1]
                    best_len = pos2-pos1
    return best_list

def resoudre(x, n, l):
    """
    :param x: le nombre magique
    :type x: int
    :param n: la longueur du code la Matriks
    :type n: int
    :param l: le code de la Matriks
    :type l: list[int]
    """
    # TODO Les deux clés (chacune sur une ligne) ou le message "IMPOSSIBLE".
    cum_tab = get_cumulative_tab(l, n)
    print(cum_tab)
    best_pos = [0,0]
    sommes = get_sommes(cum_tab, x)
    print(sommes)
    s1 = sommes[0]
    s2 = sommes[1]
    l1 = sousliste_of_somme(l, cum_tab, s1, n)
    l2 = sousliste_of_somme(l, cum_tab, s2, n)
    print(l1)
    print(l2)

if __name__ == '__main__':
    x = int(input())
    n = int(input())
    l = list(map(int, input().split()))
    resoudre(x, n, l)
