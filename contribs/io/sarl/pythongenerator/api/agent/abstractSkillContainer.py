# -*- coding: utf-8 -*-
"""
Created on Tue Oct  4 21:00:29 2022

@author: loic
"""


class AbstractSkillContainer:
    # Skill repository
    # dictionnary of capacity => AtomicSkillReference
    skillRepository = dict()

    def __init__(self):
        pass

    def getSkill(capacity):
        if capacity is not None:
            return dict(capacity)
        return None
