import abc
import uuid


class Space(abc.ABC):

    @abc.abstractmethod
    def getNumberOfStrongParticipants(self) -> int:
        # Replies the number of strong participants to the space
        pass

    @abc.abstractmethod
    def getNumberOfWeakParticipants(self) -> int:
        # Replies the number of weak participants to the space
        pass

    @abc.abstractmethod
    def getSpaceID(self):
        # Replies the Identification of this Interaction Space
        pass

    def isPseudoEmptyDefault(self) -> bool:
        # Replies if the space could be considered as empty
        return self.getNumberOfStrongParticipants() == 0

    @abc.abstractmethod
    def isPseudoEmpty(self, identifier: uuid) -> bool:
        # Replies if the space is empty or the given identifier is associated to the only one participant to the space
        pass