__author__ = 'mahandong'
import random
import csv, shutil, os, glob, cPickle
from log import strd_logger
import re

#output two dicts, while print information to the screen
def table(target,quiet = True):
    table = {}
    reverse = {}
    if isinstance(target, dict):
        for key in target.keys():
            tmp = ''
            if isinstance(target[key],list):
                if str(key) in table.keys():
                    table[str(key)] += len(target[key])
                else:
                    table[str(key)] = len(target[key])
                if not quiet:
                    print key+":"+str(len(target[key]))
                tmp = str(len(target[key]))
            if isinstance(target[key],dict):
                if str(key) in table.keys():
                    table[str(key)] += len(target[key].keys())
                else:
                    table[str(key)] = len(target[key].keys())
                if not quiet:
                    print str(key)+":"+str(len(target[key].keys()))
                tmp = str(len(target[key].keys()))
            if isinstance(target[key],str):
                if str(key) in table.keys():
                    table[str(key)] += len(target[key])
                else:
                    table[str(key)] = len(target[key])
                if not quiet:
                    print str(key)+":"+str(len(target[key]))
                tmp = str(len(target[key]))
            if tmp:
                if tmp in reverse.keys():
                    reverse[tmp] += 1
                else:
                    reverse[tmp] = 1
        if not quiet:
            print "total key numbers: ", str(len(target.keys()))
            print "reverse table: "
        for i in reverse.keys():
            if not quiet:
                print str(i)+":"+str(reverse[i])
        if not quiet:
            print 'total reverse key numbers:', str(len(reverse.keys()))

    if isinstance(target, list):
        unique = set(target)
        for i in unique:
            tmp=''
            if not quiet:
                print i+':', target.count(i)
            if i in table.keys():
                table[str(i)] += target.count(i)
            else:
                table[str(i)] = target.count(i)
            tmp = target.count(i)
            if tmp:
                if tmp in reverse.keys():
                    reverse[tmp] += 1
                else:
                    reverse[tmp] = 0
        if not quiet:
            print "reverse table: "
        for i in reverse.keys():
            if not quiet:
                print str(i)+":"+str(reverse[i])

    return table, reverse




def weightedResample1(p, w):
    try:
        #p #original objects
        #w #weight vector
        if len(p) != len(w):
            return False
        else:
            N = len(p) #total length
        p3 = []
        index = int(random.random()* N)
        beta = 0.0
        for i in range(N):
            beta += random.random() * 2.0 * max(w)
            while beta > w[index]:
                beta -= w[index]
                index = (index + 1) % N
            p3.append(p[index])
        return p3
    except Exception as e:
        #log.error(e)
        print e

def weightedResample2(p, w):
    try:
        #p #original objects
        #w #weight vector
        if len(p) != len(w):
            return False
        else:
            N = len(p) #total length
        w_norm = []
        for i in range(N):
            w_norm.append(w[i]/sum(w))
        w_cdf = []
        temp = 0
        for i in range(N):
            temp += w_norm[i]
            w_cdf.append(temp)
        p3 = []
        for i in range(N):
            seed = random.random()
            for j in range(N):
                if seed <= w_cdf[j]:
                    p3.append(p[j])
                    break
        return p3
    except Exception as e:
        #log.error(e)
        print e