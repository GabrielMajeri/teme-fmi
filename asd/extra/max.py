def max_diff (v):
    if v are 2 elemente:
        return minim, maxim, diferenta (dr - st)

    divide:
        (minim_st, maxim_st, dif_max_st) = max_diff (jumatatea stanga)
        (minim_dr, maxim_dr, dif_max_dr) = max_diff (jumatatea dreapta)

    et impera:
        return
            minim = min(minim_st, minim_dr),
            maxim = max(maxim_st, maxim_dr),
            dif_max = max(dif_max_st, dif_max_dr, maxim_dr - minim_st)
