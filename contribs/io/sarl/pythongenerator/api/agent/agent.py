# -*- coding: utf-8 -*-
"""
Created on Tue Oct  4 20:56:52 2022

@author: loic
"""

from contribs.io.sarl.pythongenerator.api.agent.abstractSkillContainer import AbstractSkillContainer


class Agent(AbstractSkillContainer):

    def __init__(self, parentID, agentID):
        super(Agent, self).__init__()
        self.__parentID = parentID
        self.__agentID = agentID

    def getID(self):
        return self.__agentID

    def getParentID(self):
        return self.__parentID

    def __str__(self) -> str:
        return "Agent{ID:" + str(self.__agentID) + ", parentID:" + str(self.__parentID) + "}"

    def isMe(self, uID):
        return uID is not None and uID == self.__agentID

