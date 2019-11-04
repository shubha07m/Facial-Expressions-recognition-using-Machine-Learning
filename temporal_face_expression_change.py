import csv
import matplotlib.pyplot as plt


def picfunc(mylist):
    myrowname = []
    i = 0
    for lno in range(1063):
        xlist = []
        ylist = []
        zlist = []
        for wno in range(301):
            if ('x' in mylist[lno][wno]) or ('y' in mylist[lno][wno]) or ('z' in mylist[lno][wno]):
                myrowname.append(mylist[lno][wno])
            elif 'x' in mylist[0][wno]:
                xlist.append(mylist[lno][wno])
            elif 'y' in mylist[0][wno]:
                ylist.append(mylist[lno][wno])
            elif 'z' in mylist[0][wno]:
                zlist.append(mylist[lno][wno])
        
        plt.scatter(xlist, ylist, 20)
        plt.gca().invert_yaxis()
        #plt.savefig('f' + str(i) + '.png')
        plt.show()
        plt.clf()
        plt.close()
        i += 1


picfunc(list(csv.reader(open("mytest.csv"))))