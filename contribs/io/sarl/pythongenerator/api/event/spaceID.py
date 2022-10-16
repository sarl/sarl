from contribs.io.sarl.pythongenerator.api.event.spaceSpecification import SpaceSpecification
import uuid
from typing import TypeVar


T = TypeVar('T', bound=SpaceSpecification)


class SpaceID(object):

    def __init__(self, contextID: uuid, identifier: uuid, spaceSpec: T):
        assert uuid is not None and identifier is not None and spaceSpec is not None
        self.__contextID = contextID
        self.__id = identifier
        self.__spaceSpec = spaceSpec

    def getContextID(self):
        return self.__contextID

    def getID(self):
        return self.__id

    def __str__(self) -> str:
        return "SpaceID{ContextID : " + str(self.__contextID) + ", ID : " + str(self.__id) + "}"

    def equals(self, spaceId):
        if self.__id == spaceId.__id:
            return True
        return False
