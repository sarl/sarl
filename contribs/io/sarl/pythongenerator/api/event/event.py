from contribs.io.sarl.pythongenerator.api.event.address import Address
import uuid


class Event(object):

    def __init__(self):
        self.__source = None

    def getSource(self):
        return self.__source

    def setSource(self, source: Address):
        self.__source = source

    def isFromAddress(self, address: Address):
        if self.__source is not None and self.__source.equals(address):
            return True
        return False

    def isFromEntity(self, entityId: uuid):
        if self.__source.getParticipantId() == entityId:
            return True
        return False

    def __str__(self) -> str:
        return "Event{" + str(self.__source) + "}"