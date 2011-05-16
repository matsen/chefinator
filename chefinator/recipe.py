#! /usr/bin/env python

cup = fractions.Fraction(1)
count = -cup
qcup = cup / 4
oz = qcup / 2
tbsp = oz / 2
tsp = tbsp / 3
etsp = tsp / 8
drop = tsp / 96
pint = cup * 2
quart = pint * 2
gallon = quart * 4

suffixes = {
    '': count,
    'cup': cup, 'cups': cup, 'c': cup,
    'tbsp': tbsp, 'T': tbsp,
    'tsp': tsp, 't': tsp,
    'oz': oz,
    'pint': pint, 'pints': pint, 'pt': pint,
    'quart': quart, 'quarts': quart, 'qt': quart,
    'gallon': gallon, 'gallons': gallon, 'gal': gallon,
}

class Quantity:

    def __init__(self, unit, amount):
        self.unit = unit
        self.amount = amount

    def __str__(self):
        return("%d of %s" % (self.unit, self.amount))



