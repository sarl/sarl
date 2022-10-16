from contribs.io.sarl.pythongenerator.api.event.spaceID import SpaceID
import abc
import uuid


class Space(abc.ABC):

    @abc.abstractmethod
    def getNumberOfStrongParticipants(self) -> int:
        # Replies the number of strong participants to the space
        pass

    @abc.abstractmethod
    def getNumberOfWeakParticipants(self) -> int:
        # Replies the number of strong participants to the space
        pass

    @abc.abstractmethod
    def getSpaceID(self) -> SpaceID:
        # Replies the Identification of this Interaction Space
        pass

    @abc.abstractmethod
    def isPseudoEmpty(self) -> bool:
        # Replies if the space could be considered as empty
        pass

    @abc.abstractmethod
    def isPseudoEmptyUUID(self, id: uuid) -> bool:
        # Replies if the space is empty or the given identifier is associated to the only one participant to the space
        pass