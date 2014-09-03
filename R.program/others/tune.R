library("tuneR")
d = 15000
bt = 16

do = sine(523, bit = bt, duration = d)
re = sine(578, bit = bt, duration = d)
mi = sine(659, bit = bt, duration = d)
fa = sine(698, bit = bt, duration = d)
so = sine(784, bit = bt, duration = d)
la = sine(880, bit = bt, duration = d)
si = sine(988, bit = bt, duration = d)
stp = silence(bit = bt, duration = d)

mu=bind(mi,mi,mi,stp,mi,mi,mi,stp,mi,so,do,re,mi,stp,stp,
        fa,fa,fa,stp,fa,mi,mi,stp,mi,re,re,do,re)
play(mu)

mu2=bind(do,do,so,so,la,la,so,stp,fa,fa,mi,mi,re,re,do)
play(mu2)


