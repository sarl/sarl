from contribs.io.sarl.pythongenerator.api.event.address import Address
import uuid


class Event(object):

    def __init__(self, source: Address = None):
        self.__source = source

    def getSource(self):
        return self.__source

    def setSource(self, source: Address):
        self.__source = source

    def isFromAddress(self, address: Address):
        return (self.__source is not None) and (self.__source.equals(address))

    def isFromEntity(self, entityId: uuid):
        return (entityId is not None) and (self.__source is not None) and (self.__source.getParticipantId() == entityId)

    def __str__(self) -> str:
        return "Event{" + str(self.__source) + "}"