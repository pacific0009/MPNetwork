import serial, re, os
from erlport.erlterms import Atom
from erlport.erlang import set_message_handler, cast
import threading
from erlport.erlang import call
MAXIMUM_NODES =32
MAX_HOP_DISTANCE =10
SIZE_OF_CS_DATA =18
SIZE_OF_DATA =8
SIZE_OF_HEADER =10
SIZE_OF_SN =4
START_INDEX_DATA =10
MAX_RESERVE_SEQUENCE =10
DISTANCE_VECTOR_SN =0
MPN_SN =1
PING_SN =3
SUBSCRIBERS = []

def register_handler(port, baudrate):
    SER = connect( port, baudrate)
    if not SER:
        return (Atom(b'failed'), Atom(b'serial connection failed'))
    def handler(message):
        publish(SER, message)
    set_message_handler(handler)
    t = threading.Thread(target=on_serial, args=[SER])
    t.setDaemon(True)
    t.start()
    return (Atom(b'ok'), Atom(b'connected'))

def subscribe(subscriber):
    if not subscriber in SUBSCRIBERS:
        SUBSCRIBERS.append(subscriber)
    return (Atom(b'ok'), Atom(b'subscribed'))


def unsubscribe(subscriber):
    if subscriber in SUBSCRIBERS:
        SUBSCRIBERS.remove(subscriber)
    return (Atom(b'ok'), Atom(b'unsubscribed'))


class RFPACKET:
    def __init__(self):
        self.serialNo=0
        self.next_hop = MAXIMUM_NODES
        self.destination = MAXIMUM_NODES
        self.source = MAXIMUM_NODES
        self.data = list()
    def __str__(self):
        return "Sn: {}, NH: {}, Dt: {}, Sr: {}, Dt: {}".format(self.serialNo, self.next_hop, self.destination, self.source, self.data)


def packet_decode(rf_string):
    if len(rf_string) < SIZE_OF_CS_DATA:
        return None
    # print("\t\tDecoding: {}".format(rf_string))
    cs_str = rf_string[19:21]
    received_cs = int(cs_str, 16)
    #print("CS: {}".format(cs_str))
    calculated_XRCS = 0
    byte_arr = bytearray(rf_string.encode())#bytes(rf_string, 'ascii')
    for i in range(SIZE_OF_CS_DATA):
        calculated_XRCS ^= byte_arr[i + 1]
    if calculated_XRCS != received_cs:
        print("\t\tCS Invalid: Received({}) Calculated({})\n".format(received_cs, calculated_XRCS))
        return None
    received_packet = RFPACKET()
    received_packet.serialNo = int(rf_string[1:5], 16)
    received_packet.next_hop = int(rf_string[5:7], 16)
    received_packet.destination = int(rf_string[7:9], 16)
    received_packet.source = int(rf_string[9:11], 16)
    received_packet.data = rf_string[11:19]
    return received_packet

def packet_encode(response):
    packet = "<"
    packet += '{:04x}'.format(response.serialNo)
    packet += '{:02x}'.format(response.next_hop)
    packet += '{:02x}'.format(response.destination)
    packet += '{:02x}'.format(response.source)
    for i in range(SIZE_OF_DATA):
        packet += response.data[i]
    calculated_XRCS = 0
    byte_arr = bytearray(packet.encode())#bytes(packet, 'ascii')
    for i in range(SIZE_OF_CS_DATA):
        calculated_XRCS ^= byte_arr[i + 1]
    packet += '{:02x}'.format(calculated_XRCS)
    packet += ">"
    return packet


def connect(port, baud):
    ser = serial.Serial(port, baudrate=baud, timeout=None)
    print("connected")
    return ser

def on_serial(SER):
    p = re.compile('<.*>')
    while True:
        try:
            packet = str(SER.readline())
        except:
            call(Atom("postman_service"), Atom("stop"), [])
        result = p.search(packet)
        if result:
            #print("\t<--: {}".format(result.group(0)))
            rf_packet = packet_decode(result.group(0))
            print("<-- {} encoded:{}\n".format(rf_packet, result.group(0)))
            if rf_packet :
                message = (rf_packet.serialNo,
                           rf_packet.next_hop,
                           rf_packet.destination,
                           rf_packet.source,
                           rf_packet.data)
                for subscriber in SUBSCRIBERS:
                    cast(subscriber, message)


def publish(ser, message):
    packet = RFPACKET()
    #try:
    packet.serialNo, packet.next_hop, packet.destination, packet.source, packet.data = message
    encoded_packet = packet_encode(packet)
    print("--> {} encoded:{}\n".format(packet, encoded_packet))
    if ser.write(encoded_packet):
        return (Atom(b'ok'), Atom(b'published to serial'))
    else:
        print("disconnected")
        return (Atom(b'failed'),  Atom(b'Unavailable'))