% process definitions

defproc(producerConsumerSyst, producer | consumer | faults $ bufferS0).
defproc(producer, notFull > produce > producer).
defproc(consumer, notEmpty > consume > consumer).
defproc(faults, underflow ? overflow).
defproc(bufferUF, notFull > produce > bufferUF ? produce > bufferUF ? 
    consume > bufferUF).
defproc(bufferS0, notFull > produce > bufferS1 ? produce > bufferS1 ? 
    consume > underflow > bufferUF).
defproc(bufferS1, notFull > produce > bufferS2 ? produce > bufferS2 ? 
    consume > bufferS0 ? notEmpty > consume > bufferS0).
defproc(bufferS2, notFull > produce > bufferS3 ? produce > bufferS3 ? 
    consume > bufferS1 ? notEmpty > consume > bufferS1).
defproc(bufferS3, produce > overflow > bufferOF ? consume > bufferS2 ?
    notEmpty > consume > bufferS2).
defproc(bufferOF, produce > bufferOF ? consume > bufferOF ? notEmpty > 
    consume > bufferOF).
defproc(producerConsumerSystBuggy, producerB | consumerB | faults $ bufferS0).
defproc(producerB, produce > producerB).
defproc(consumerB, consume > consumerB).
