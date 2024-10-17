# Уровень 7

А что вы думаете о спец-комментариях — XML-документации и комментариях с историей изменений файла?

Исходный код:
```cs
/*Changes (from 11-Oct-2011)
* --------------------------
* 12-Sep-2011 : Fix bug
* 11-Oct-2011 : Move implementation to another file
* 05-Nov-2011 : Add XML comments 
*/
namespace Logger
{
    ///<summary>Implement Logger to provide customized event filtering</summary>
    ///<remarks>
    ///<para>
    ///Users should implement this interface to implement customized logging
    ///event filtering. Note that <see cref="Logger.Repository.Hierarchy.Logger"/>
    ///and <see cref="Logger.Appender.AppenderSkeleton"/>, the parent class of all
    ///standard appenders, have built-in filtering rules. It is suggested that you
    ///first use and understand the built-in rules before rushing to write
    ///your own custom filters.
    ///</para>
    ///</remarks>
    public interface IFilter : IOptionHandler
    {
        ///<summary>Make a decision about logging event.</summary>
        ///<param name="loggingEvent">The LoggingEvent to decide upon</param>
        ///<returns>The decision of the filter</returns>
        FilterDecision Decide(LoggingEvent loggingEvent);
    
        ///<summary>Property to get and set the next filter</summary>
        ///<value>The next filter in chain</value>
        IFilter NextInChain { get; set; }
    }
}
```

Исправленный код:
```cs
namespace Logger
{
    ///<summary>Implement Logger to provide customized event filtering</summary>
    ///<remarks>
    ///<para>
    ///Users should implement this interface to implement customized logging
    ///event filtering. Note that <see cref="Logger.Repository.Hierarchy.Logger"/>
    ///and <see cref="Logger.Appender.AppenderSkeleton"/>, the parent class of all
    ///standard appenders, have built-in filtering rules. It is suggested that you
    ///first use and understand the built-in rules before rushing to write
    ///your own custom filters.
    ///</para>
    ///</remarks>
    public interface IFilter : IOptionHandler
    {
        FilterDecision Decide(LoggingEvent loggingEvent);
    
        IFilter NextInChain { get; set; }
    }
}
```

Объяснения:
- Когда-то очень давно был смысл писать комментарии с историей изменения файла. Но сейчас вместо таких комментариев лучше использовать систему контроля версий и писать понятные сообщения к коммитам.
- XML-комментарии не несущие новой информации бесполезны.
- Не пишите XML-комментарий только для того, чтобы он был. В наличии комментария должен быть какой-то смысл.
