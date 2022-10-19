from contribs.io.sarl.pythongenerator.api.event.spaceSpecification import SpaceSpecification
import uuid
from typing import TypeVar

T = TypeVar('T', bound=SpaceSpecification)


class SpaceID(object):

    def __init__(self, contextID: uuid, identifier: uuid, spaceSpec: T):
        assert (contextID is not None) and (identifier is not None)
        self.__contextID = contextID
        self.__id = identifier
        self.__spaceSpec = spaceSpec

    def getContextID(self):
        return self.__contextID

    def getID(self):
        return self.__id

    def getSpaceSpecification(self):
        return self.__spaceSpec

    def __str__(self) -> str:
        return "SpaceID{ContextID : " + str(self.__contextID) + ", ID : " + str(self.__id) + "}"

    def equalsContext(self, spaceId):
        if self.__contextID is None:
            return spaceId.getContextID() is None
        return self.__contextID == spaceId.getContextID()

    def equalsID(self, spaceId):
        if self.__id is None:
            return spaceId.getID() is None
        return self.__id == spaceId.getID()

    def equals(self, spaceId):
        if spaceId is None:
            return False
        if not self.equalsContext(spaceId):
            return False
        return self.equalsID(spaceId)
