from contribs.io.sarl.pythongenerator.api.event.space import Space
from contribs.io.sarl.pythongenerator.api.event.spaceSpecification import SpaceSpecification
from typing import TypeVar, Generic
import abc
import uuid

S = TypeVar('S', bound=Space)
T = TypeVar('T', bound=SpaceSpecification)


class AgentContext(abc.ABC):

    @abc.abstractmethod
    def getId(self) -> uuid:
        # Replies the identifier of the context
        pass

    @abc.abstractmethod
    def getDefaultSpace(self):
        # Replies the default space of the context
        pass

    # Maybe we can do T[S] to specify that the parameter should extend SpaceSpecification[S] but i'm not sure
    @abc.abstractmethod
    def getSpaces(self, *spec: T) -> S:
        # Replies all the spaces that are implementing the given specification if not None
        pass

    # Maybe we can do T[S] to specify that the parameter should extend SpaceSpecification[S] but i'm not sure
    @abc.abstractmethod
    def createSpace(self, spec: T, spaceUUID: uuid, *creationParams) -> S:
        # Create an instance of space following the given specification
        pass

    # Maybe we can do T[S] to specify that the parameter should extend SpaceSpecification[S] but i'm not sure
    @abc.abstractmethod
    def getOrCreateSpaceWithSpec(self, spec: T, spaceUUID: uuid, *creationParams) -> S:
        # Retrieve or create an instance of space which was created with the given specification
        pass

    # Maybe we can do T[S] to specify that the parameter should extend SpaceSpecification[S] but i'm not sure
    @abc.abstractmethod
    def getOrCreateSpaceWithId(self, spec: T, spaceUUID: uuid, *creationParams) -> S:
        # Retrieve or create an instance of space with the given identifier
        pass

    @abc.abstractmethod
    def getSpace(self, spaceUUID: uuid) -> S:
        # Retrieve, but do not create, an instance of space following the given ID
        pass

    @abc.abstractmethod
    def isRootContext(self) -> bool:
        # Replies if the context is a root context
        pass
