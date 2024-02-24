import qualified SFDataStructures.Stacks.MNStack as S
import qualified SFDataStructures.Queues.MSQueue as Q
import System.IO

main :: IO ()
main = do


    -- Set up the input and output buffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin LineBuffering


    -- STACK TESTING --
    putStrLn ""
    putStrLn "Testing Stacks..."
    putStrLn ""


    -- Create a stack
    let stack1 = S.empty :: S.MNStack Int
    print stack1
    putStrLn ""


    -- Push
    let stack2 = S.push 1 stack1
    print stack2
    putStrLn ""


    -- Singleton
    let stack2a = S.singleton 1
    print stack2a
    putStrLn ""


    -- Pop
    let stack3 = S.pop stack2
    print stack3
    putStrLn ""


    if S.isEmpty stack3 && stack3 == stack1 && stack2 == stack2a
        then putStrLn "Good isEmpty, create, singleton, push and pop stack"
        else error "Bad isEmpty, create, singleton, push or pop stack"
    putStrLn "" -- I'm testing a lot of things here because it's a simple data structure


    -- fromList, toList, top, size


    let stackList1 = [1..5]
    let stackTop = head stackList1
    let stackSize = length stackList1
    let stack4 = S.push (1 :: Integer) $ S.push 2 $ S.push 3 $ S.push 4 $ S.push 5 S.empty
    let stack5 = S.fromList stackList1
    let stackList2 = S.toList stack5


    if stack4 == stack5
        then putStrLn "Good fromList stack"
        else error "Bad fromList stack"
    putStrLn ""


    if stackList1 == stackList2
        then putStrLn "Good fromList and toList stack"
        else error "Bad fromList or toList stack"
    putStrLn ""


    if stackTop == S.top stack5 && stackSize == S.size stack5
        then putStrLn "Good top and size stack"
        else error "Bad top or size stack"
    putStrLn ""


    putStrLn "-------------------"



    -- QUEUE TESTING --
    putStrLn ""
    putStrLn "Testing Queues..."
    putStrLn ""


    -- Create a queue
    let queue1 = Q.empty :: Q.MSQueue Int
    print queue1
    putStrLn ""


    -- Enqueue
    let queue2 = Q.enqueue 1 queue1
    print queue2
    putStrLn ""


    -- Dequeue
    let queue3 = Q.dequeue queue2
    print queue3
    putStrLn ""


    if Q.isEmpty queue3 && queue3 == queue1
        then putStrLn "Good isEmpty, create, enqueue and dequeue queue"
        else error "Bad isEmpty, create, enqueue or dequeue queue"
    putStrLn "" -- I'm testing a lot of things here because it's a simple data structure


    -- fromList, toList, front, size


    let queueList1 = [1..5]
    let queueFirst = head queueList1
    let queueSize = length queueList1
    let queue4 = Q.enqueue (5 :: Integer) $ Q.enqueue 4 $ Q.enqueue 3 $ Q.enqueue 2 $ Q.enqueue 1 Q.empty
    let queue5 = Q.fromList queueList1
    let queueList2 = Q.toList queue5


    if queue4 == queue5
        then putStrLn "Good fromList queue"
        else error "Bad fromList queue"
    putStrLn ""


    if queueList1 == queueList2
        then putStrLn "Good fromList and toList queue"
        else error "Bad fromList or toList queue"
    putStrLn ""


    if queueFirst == Q.first queue5 && queueSize == Q.size queue5
        then putStrLn "Good front and size queue"
        else error "Bad front or size queue"
    putStrLn ""

    
    putStrLn ""
    putStrLn "-------------------"
    putStrLn "Finished testing..."
    putStrLn "-------------------"
