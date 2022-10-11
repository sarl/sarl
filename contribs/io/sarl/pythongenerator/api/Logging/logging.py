# -*- coding: utf-8 -*-
"""
Created on Tue Oct  4 20:56:52 2022

@author: loic
"""


class Logging:

    def __init__(self):
        self.name = ""

    def setLoggingName(self, name):
        self.name = name

    def info(self):
        print(self.name)
