# printing leads with what was not compared

    Code
      print(comparison)
    Message
      <scans_comparison> Support assistant baseline -> Support assistant candidate
      Key: sample_id and epoch; dataset version support-v1
      Baseline snapshot: sha256:baseline
      Candidate snapshot: sha256:candidate
      
      -- Not compared 
      ! 1 trajectory only in the baseline
      ! 2 trajectories only in the candidate
      ! 2 outcomes missing on a side
      ! 3 pattern checks not comparable (not assessed, not run, or changed scanner)
      ! 5 resource values recorded on a side but not comparable
      
      -- Paired changes in 7 matched cases 
      * Outcome exact_scorer (version 1): 2 improved, 1 regressed, 3 unchanged;
        counts only
      * Pattern event_error: appeared in 1, resolved in 0, persisted in 0; counts
        only
      * Pattern turn_error: appeared in 1, resolved in 0, persisted in 0; counts only
      * Resource input_tokens: increased in 2, decreased in 0, median change 0
        tokens; counts only
      * Resource output_tokens: increased in 2, decreased in 0, median change 0
        tokens; counts only
      * Resource turn_duration: increased in 2, decreased in 0, median change 0 s;
        counts only

---

    Code
      print(unpaired)
    Message
      <scans_comparison> Support assistant baseline -> Support assistant candidate
      Key: sample_id and epoch; dataset version not declared
      Baseline snapshot: sha256:baseline
      Candidate snapshot: sha256:candidate
      
      -- Not compared 
      ! 17 trajectories missing a key value
      
      -- Unpaired description 
      No dataset version was declared, so no case can be matched.
      See `$summaries` for each side. No paired estimate is produced.

