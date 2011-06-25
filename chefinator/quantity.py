#!/usr/bin/env python

class Quantity:

    def __init__(self, unit, amount):
        self.unit = unit
        self.amount = amount

    def __str__(self):
        return("%d of %s" % (self.unit, self.amount))



