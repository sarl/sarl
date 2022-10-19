from contribs.io.sarl.pythongenerator.api.event.spaceID import SpaceID
import uuid


class Address(object):

    def __init__(self, spaceId: SpaceID, participantId: uuid):
        assert (spaceId is not None) and (participantId is not None)
        self.__spaceId = spaceId
        self.__participantId = participantId

    def getParticipantId(self):
        return self.__participantId

    def getSpaceId(self):
        return self.__spaceId

    def __str__(self) -> str:
        return "Address{" + str(self.__spaceId) + ", ParticipantId : " + str(self.__participantId) + "}"

    def equals(self, address):
        return (address is not None) and (self.__spaceId.equals(address.getSpaceId())) and\
               (self.__participantId == address.getParticipantId())