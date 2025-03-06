<config model="o1"  max_tokens="40000" reasoning_effort="high"/>
<config model="gpt-4.5"  max_tokens="10000" />


<msg role="user">
I am developing a file based openai chat interface for conversations with the chat ai. Basically I am trying to develop a powerful interface for interacting with openai ai chat models that only requires a file with xml styled markup that represents a conversation. Here is an example photo of a conversation using the application:

<img src="/Users/dakotamurphy/Desktop/chat.png" local/>

A file represents a single conversation, messages are deliminated by 'msg' elements with information about the message like role, ect in the element attributes. The xml file acts as the ui, and the user adds messages to the file then runs the application and the ai response is then appended to file, essentially the file acts as a repl. I also have 'img' and 'doc' elements to make it easier to add content to a message inline without manual copy/paste. The application gets the doc/image from the src url and provide the document/image in the message sent to the chat model. The application also includes function tools for the chat model to use to help with user queries, the application defines these tools and outputs the tool responses inside the conversation. the ai model will only get the contents of msg elements, it is the applications job to proccess non msg elements and update the contents of the conversation messages that are sent to the chat model. 

So far the application works well with this simple implementation but I am looking to improve apon it and make a robust document based chat interface that targets power users and novices that want a light weight chat interface that does not rely on web apps or installed ui interfaces but unlocks the full power of the openai chat api all with the simplicity of interfacing with a simple file. It acts as a high level DSL and repl all in one. basically you could open the file in vscode, add a message, run the program and see the ai respons in realtime in the same file 

Come up with a proposal for new features for the application that will help accomplish the stated goals above. Think about things that help improve the ergonomics of using the application (i.e things that reduce copy paste, easily build chat context, call other agents to help generate content for a msg, configure chat api configurations, optionally cache dynamically created content so for subsiquent runs of a conversation the application does not recreate content for dynamic elements and instead retreives from cache, ect). Be very specific and include any details that would help in the implementation of the proposals. Remember the workflow, this is an interactive document and each run streams live a single output from the assistant except in the case of a function call where the a run would produce a assistant message to call a function, a function result msg containg the function call result, a assistant message with a response given the result of the function call, and if the response is another ssistant requested function call the proccess repeats. So keep this in mind when coming up with features and make sure the feature makes sense given the workflow. Also remember function calling is based on predefined functions that are added to the api call and openai automatically inserts the availible functions into the system prompt so we do not need to define functions in user messages. 

use the following docs as a reference and as inspiration to what the chat api can do
----
Here is documentation on chat text generation:
<doc src="https://platform.openai.com/docs/guides/text-generation" strip/>

Here is documentation on function calling:
<doc src="https://platform.openai.com/docs/guides/function-calling" strip/>

Here is documentation on reasoning best practices:
<doc src="https://platform.openai.com/docs/guides/reasoning-best-practices" strip/>
</msg>