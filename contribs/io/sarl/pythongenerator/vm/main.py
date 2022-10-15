from contribs.io.sarl.pythongenerator.vm.GuardedAgent.guardedAgent import GuardedAgent
#from contribs.io.sarl.pythongenerator.vm.HelloWorld.helloWorld import HelloWorldAgent
from contribs.io.sarl.pythongenerator.vm.LoggingAgent.LoggingAgent import LoggingAgent

### HelloWorld
# helloWorld = HelloWorldAgent
# eventsList = helloWorld.__guard_io_sarl_core_Initialize__(helloWorld,1)
# eventsList[0](helloWorld,1)

### HelloWorld with the loggingSkill
loggingAgent = LoggingAgent(1, 2)
myAgentEvents = loggingAgent.__guard_io_sarl_core_Initialize__(1)
myAgentEvents[0](1)

### GuardedAgent
guardedAgent = GuardedAgent(1, 2)
myAgentEvents1 = guardedAgent.__guard_io_sarl_core_Initialize__(2)
myAgentEvents1[0](2)
