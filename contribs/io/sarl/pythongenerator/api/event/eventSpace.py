from contribs.io.sarl.pythongenerator.api.event.space import Space
from contribs.io.sarl.pythongenerator.api.event.event import Event
from contribs.io.sarl.pythongenerator.api.event.address import Address
import uuid
import abc


# Given that EventSpace implements Space, we do not need to specify that EventSpace inherits ABC
class EventSpace(Space):

    @abc.abstractmethod
    def emit(self, eventSource: uuid, event: Event):
        # Emits the event inside this space
        self.emitWithScope(eventSource, event, None)

    @abc.abstractmethod
    def emitWithScope(self, eventSource: uuid, event: Event, scope):
        # Emits the event inside this space with the given scope
        pass

    @abc.abstractmethod
    def getAddress(self, identifier: uuid) -> Address:
        # Returns the address of the agent identified by id
        pass