from contribs.io.sarl.pythongenerator.vm.helloAgent import HelloAgent
from contribs.io.sarl.pythongenerator.vm.helloWorld import HelloWorldAgent

### HelloWorld
helloWorld = HelloWorldAgent
eventsList = helloWorld.__guard_io_sarl_core_Initialize__(helloWorld,1)
eventsList[0](helloWorld,1)


### HelloWorld with the loggingSkill
"""
myAgent = HelloAgent
myAgentEvents = myAgent.__guard_io_sarl_core_Initialize__(myAgent,1)
myAgentEvents[0](myAgent,1)
"""

