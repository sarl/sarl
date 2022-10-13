# -*- coding: utf-8 -*-
"""
Created on Tue Oct  4 21:00:29 2022

@author: loic
"""


class AbstractSkillContainer(object):

    def __init__(self):
        # Skill repository
        # dictionnary of capacity => AtomicSkillReference
        self.__skillRepository = dict()
        pass

    def getSkill(self, capacity):
        if capacity is not None:
            return self.__skillRepository.get(capacity.__class__)
        return None

    def setSkill(self, skill, *capacities):
        self.__setSkill(skill, True, capacities)

    def __setSkill(self, skill, ifAbsent, *capacities):
        if capacities is None or len(capacities) == 0:
            pass
        else :
            for cap in capacities:
                self.__registerSkill(skill, ifAbsent, cap)

    def __registerSkill(self, skill, ifAbsent, capacity):
        if isinstance(skill, capacity):
            self.__skillRepository[capacity.__class__] = skill
        else:
            raise Exception("the skill must implement the given capacity " + capacity.__class__)
