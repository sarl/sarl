from contribs.io.sarl.pythongenerator.api.event.spaceID import SpaceID
import abc
from typing import TypeVar, Generic


T = TypeVar('T', bound=SpaceID)


class SpaceSpecification(abc.ABC, Generic[T]):

    @abc.abstractmethod
    def create(self, id: SpaceID, *params) -> T:
        # Creates a Space that respects this specification
        pass