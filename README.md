# CHS2010
## Replication of _Cunha, Heckman and Schennach (2010)_


### The Perfect Commit

A good commit must have at least the following two components.
- Add the right changes.
- Compose a good message

Our goal is to create a commit that makes sense, one that **only includes changes from a single topic**. This is the golden rule of commits. The opposite of what we mean by *single topic* is a commit that gathers all the local changes we have made. The more topics into a single commit, the harder it is to understand what has been modified (both for your colleages and yourself). Therefore, even if you have made multiple changes in your local repository, only commit smaller, single-topic chunks that are easy to read and understand.

Let's now elaborate on the second part of a perfect commit, *i.e.*, the message. There are two parts:

1. **Subject Line**. Brief summary of what was modified (less than 80 characters). Golden rule: if you have trouble summarizing what happened in less than 80 characters, it probably means there are multiple topics in the commit, so the subject line is somewhat a sanity check for the first rule.
2. **Body**. This is a more detailed explanation. A good rule would be to answer the questions *what?* and *why?* these changes. Lastly, add some extra information of potential drawbacks, caveats, or things to keep in mind for the future.

Open your favorite Terminal and simply type something like
```sh
git commit -m "This is the Subject Line" -m "This is the body"
```

