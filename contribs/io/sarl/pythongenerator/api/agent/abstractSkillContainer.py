# -*- coding: utf-8 -*-
"""
Created on Tue Oct  4 21:00:29 2022

@author: loic
"""

from contribs.io.sarl.pythongenerator.api.capacity.capacity import Capacity
from contribs.io.sarl.pythongenerator.api.agent.skill import Skill


class AbstractSkillContainer(object):

    def __init__(self):
        # Skill repository
        # dictionnary of capacities
        self.__skillRepository = dict()

    def getSkill(self, capacity):
        assert capacity is not None and issubclass(capacity, Capacity) and not issubclass(capacity, Skill)
        skill = self.__skillRepository.get(capacity)
        assert skill is not None
        return skill

    def setSkill(self, skill, *capacities):
        if capacities is None or len(capacities) == 0:
            for cls in type(skill).__mro__:
                if cls != Capacity and cls != skill.__class__ and issubclass(cls, Capacity) and not issubclass(cls,
                                                                                                               Skill):
                    self.__registerSkill(skill, cls)
        else:
            for cap in capacities:
                assert not issubclass(cap, Skill)
                self.__registerSkill(skill, cap)

    def __registerSkill(self, skill, capacity):
        if issubclass(capacity, Capacity):
            if isinstance(skill, capacity):
                self.__skillRepository[capacity] = skill
            else:
                raise Exception("the skill must extend the given capacity " + str(capacity))
        else:
            raise Exception("the capacity provided must extend the given class " + str(Capacity))

    def hasSkill(self, capacity):
        assert capacity is not None
        return capacity in self.__skillRepository.keys()

    def clearSkill(self, capacity):
        assert capacity is not None
        self.__skillRepository.pop(capacity, None)
