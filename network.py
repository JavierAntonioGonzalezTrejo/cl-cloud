import sys, ssl, pika, ssl, uuid
from netaddr import IPNetwork
# Javier Antonio Gonzalez Trejo 29100317
def networkRun(canal, metodo, propiedades, cuerpo):
    """Function that runs network operations"""
    cuerpoList = str(cuerpo).split(" ")
    commandRecived = cuerpoList[0][2:]
    print (commandRecived)
    if commandRecived == makeNetCommand:
        result = makeNet(cuerpoList[1:])
        sendResultOperation(result, propiedades.reply_to, propiedades.correlation_id)
    elif commandRecived == listNetCommand:
        result = listNet(netList)
        sendResultOperation(result, propiedades.reply_to, propiedades.correlation_id)
    elif commandRecived == bindIPInstanceCommandNetwork:
        result = bindIP(cuerpoList[1:], canal)
        sendResultOperation(result, propiedades.reply_to, propiedades.correlation_id)
    elif commandRecived == unbindIPInstanceCommand:
        result = unbindIP(cuerpoList[1])
        sendResultOperation(result, propiedades.reply_to, propiedades.correlation_id)
    elif commandRecived == listIPInstanceCommand:
        result = listIPInstance()
        sendResultOperation(result, propiedades.reply_to, propiedades.correlation_id)
    canal.basic_ack(delivery_tag =metodo.delivery_tag)

def listIPInstance():
    """Returns a string containing all the binded instances whit theirs ip on the following forma. 'Instance: ~s. IP: ~s."""
    stringInstanceIP = ""
    for instance in instanceBindList:
        stringInstanceIP = stringInstanceIP + "---Instance: " + instance["name"] + ". IP: " + instance["ip"] + ".\n"
    return stringInstanceIP

def unbindIP(nameInstance):
    """Removes the ip binded to the instance provided"""
    global instanceBindList
    name = nameInstance.strip("'").upper()
    indexInstance = findInstanceBind(name)
    if indexInstance == -1:
        return "Instances is not binded"
    del instanceBindList[indexInstance]
    return name + " has been unbinded!!!"

def bindIP(parameters, canal):
    """Binds an ip from an specific net to an specific instance. Needs to comunicate whit the compute node in order to bind the ip. Returns three posible outcomes. The name of the instances if succeds, 'Instance not found', or 'Net not found'.
    The parameters: 'name-instance name-net"""
    nameNet = parameters[1].strip("'").upper()
    nameInstance = parameters[0].upper()
    net = findNet(nameNet)
    if not(findInstanceBind(nameInstance) == -1):
        return "The Instances has an IP already"
    elif net == None:
        return "Net does not exist!!!"
    try:
        global instanceBindList
        instanceIP = {"name":nameInstance, "ip":next(net)}
        instanceBindList.append(instanceIP)
        return "Instance: " + nameInstance + " IP: " + instanceIP["ip"]
    except:
        return "Net out of IPs."
    return ""
            
def netIterator(net):
    """An iterator to get the network ips"""
    for i in range(1, len(net) - 2):
        yield str(net[i])
    

def listNet(netList):
    """Returns a string containing all the nets available n the following format.'---Name net: ~s. Net: ~s' """
    result = ""
    for net in netList:
        result = result + "---Name of the net: " + net["name"] + ". Net: " + net["netString"] + "\n"
    return result
 
def makeNet(message):
    """Function to create a net
    The following is format in which the parameters have to be given:
   [nameNet,ipNet,cird]
    Uses the netList to save a dictionary to identify the net
    {name, net}
    Where name is the name of the net and net is an iterator which gives the next ip to bind.
    Returns the name of the net if succeds. If the net already exists returns 'Net already exists'. Finally if it is not possible to generate the net the function returns 'Error trying to make the net"""
    try:
        nameNet = message[0].upper()
        global netList
        net = message[1] + "/" + message[2].strip("'") # When the message is converted to string, it leaves the character ' at the end of the last string.
        resultFind = findNet(nameNet)
        if not(resultFind == None):
            return "Net already exists."
        netObject = IPNetwork(net)
        netList.append({"name":nameNet, "net":netIterator(netObject), "netString":str(netObject) })
        return nameNet
    except:
        return "Error trying to make the net."
    return ""

def findNet(nameNet): 
    """Checks if the net already exists on the netList variable."""
    global netList
    print (nameNet)
    for net in netList:
        if nameNet == net["name"]:
            return net["net"]
    return None

def findInstanceBind(nameInstance):
    """Checks if the instances is already binded. Returns the index if it finds it, else returns -1"""
    for i in range(0, len(instanceBindList)):
        if instanceBindList[i]["name"] == nameInstance:
            return i
    return -1

        
def sendResultOperation(result, queueRoutingKey, correlationID):
    credentials = pika.PlainCredentials('network', 'network')
    conexion = pika.BlockingConnection(
        pika.ConnectionParameters(host="debianProblem",
                                  port = 5671,
                                  virtual_host = "cloud",
                                  ssl=True,
                                  ssl_options=dict(
                                      ssl_version = ssl.PROTOCOL_TLSv1,
                                      ca_certs="./testca/cacert.pem",
                                      keyfile="./client/key.pem",
                                      certfile="./client/cert.pem",
                                      cert_reqs=ssl.CERT_REQUIRED
                                  )))

    canal = conexion.channel()
    canal.queue_declare(queue=queueRoutingKey)
    canal.basic_publish(exchange="",
                        routing_key=queueRoutingKey,
                        properties=pika.BasicProperties(correlation_id = correlationID,
                                                        delivery_mode = 2),
                        body=result)

netList = []
instanceBindList = []

makeNetCommand = "make-net"
bindIPInstanceCommandNetwork = "bind-ip-instance-network"
listNetCommand = "list-net"
unbindIPInstanceCommand = "unbind-ip-instance"
listIPInstanceCommand = "list-ip-instance"

exchangePetition = "petition"

credentials = pika.PlainCredentials('network', 'network')
conexion = pika.BlockingConnection(
    pika.ConnectionParameters(host="debianProblem",
                              port = 5671,
                              virtual_host = "cloud",
                              ssl=True,
                              ssl_options=dict(
                                  ssl_version = ssl.PROTOCOL_TLSv1,
                                  ca_certs="./testca/cacert.pem",
                                  keyfile="./client/key.pem",
                                  certfile="./client/cert.pem",
                                  cert_reqs=ssl.CERT_REQUIRED
)))

canal = conexion.channel()
canal.exchange_declare(exchange=exchangePetition,
                         exchange_type='fanout')
queueNetwork = canal.queue_declare(exclusive=True)
queueNetworkName = queueNetwork.method.queue
canal.queue_bind(exchange=exchangePetition,
                   queue=queueNetworkName)
canal.basic_consume(networkRun, queue=queueNetworkName)
canal.start_consuming()
