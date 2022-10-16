#### GENERATED PYTHON CODE

# from io.sarl.core import Logging
# from io.sarl.lang.core import Agent
#
# class GuardedAgent(Agent,object):
#     def __on_Initialize__(self, occurrence):
#         print(u"Initialization without parameters")
#     def __on_Initialize___1(self, occurrence):
#         print(u"Initialization with parameters: " + occurrence.parameters)
#     def __guard_io_sarl_core_Initialize__(self, occurrence):
#         it = occurrence
#         __event_handles = list
#         if occurrence.parameters.isEmpty():
#             __event_handles.add(__on_Initialize__)
#         if (occurrence.parameters.isEmpty() == False):
#             __event_handles.add(__on_Initialize___1)
#         return __event_handles
#     def __init__(self):
#         pass


#### GENERATES PYTHON CODE WITH NECESSARY MODIFICATION

from contribs.io.sarl.pythongenerator.api.agent.agent import Agent


class GuardedAgent(Agent, object):
    def __on_Initialize__(self, occurrence):
        print(u"Initialization without parameters")

    def __on_Initialize___1(self, occurrence):
        print(u"Initialization with parameters: " + str(occurrence))

    def __guard_io_sarl_core_Initialize__(self, occurrence):
        it = occurrence
        __event_handles = list()
        if occurrence == 1:
            __event_handles.append(self.__on_Initialize__)
        if occurrence == 2:
            __event_handles.append(self.__on_Initialize___1)
        return __event_handles

    def __init__(self, parentID, agentID):
        super().__init__(parentID, agentID)
