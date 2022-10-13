from contribs.io.sarl.pythongenerator.vm.HelloWorld.helloWorld import HelloWorldAgent
from contribs.io.sarl.pythongenerator.vm.LoggingAgent.LoggingAgent import LoggingAgent

### HelloWorld
# helloWorld = HelloWorldAgent
# eventsList = helloWorld.__guard_io_sarl_core_Initialize__(helloWorld,1)
# eventsList[0](helloWorld,1)

### HelloWorld with the loggingSkill
loggingAgent = LoggingAgent
myAgentEvents = loggingAgent.__guard_io_sarl_core_Initialize__(loggingAgent,1)
myAgentEvents[0](loggingAgent,1)