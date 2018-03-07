
(defglobal ?*upper_limit* = 99999)
(import nrc.fuzzy.*)

(import nrc.fuzz.jess.*)

(load-package nrc.fuzzy.jess.FuzzyFunctions)

(deftemplate Business_loan_applicant
    (slot applicant_name)
     ; Applicant's age in number(Integer VALUE only)
    (slot age (type INTEGER))
    ; Amount currently owed by applicant in Dollars(Integer VALUE only)
    (slot current_owe (type INTEGER))
    ; Any pending payment of previosuly sanctioned loans?
    (slot paid (allowed-values Yes No))
    ; Applicant's Years of experience in related business 
    (slot exp (type FlOAT))
    ; Total number of businesses owned by applicant
    (slot bus (type INTEGER))
    ; Years of education of an applicant
    (slot edu (type INTEGER))
    ; Total amount invested so far by the applicant in the business
    (slot inv (type INTEGER))
    ; Ability to repay loan
    (slot repay (allowed-values Good Poor))
    ; Management Ability of an applicant
    (slot mgmt (allowed-values Good Average Poor))
    ; Credit History of an applicant
    (slot credit (allowed-values Good Poor))       
    )

(deftemplate Bank
    (slot bank_name)
    ; policy for Repayment ability
    (slot repay_ability (allowed-values Good Poor))
    ; policy for Management ability
    (slot mgmt_ability (allowed-values Good Average Poor))
    ; policy for credit history
    (slot credit_history (allowed-values Good Poor))
    )


;---------------------------------------------
; FUZZY TEMPLATE
;---------------------------------------------

(deftemplate current_owe
    "Auto-generated"
    (declare (ordered TRUE)))

(deftemplate experience
    "Auto-generated"
    (declare (ordered TRUE)))

(deftemplate business_owned
    "Auto-generated"
    (declare (ordered TRUE)))

(deftemplate education
    "Auto-generated"
   (declare (ordered TRUE)))

(deftemplate amount_invested
    "Auto-generated"
   (declare (ordered TRUE)))


;---------------------------------------------
; GLOBAL VARIABLES USED BY THE RULES
;---------------------------------------------

(defglobal ?*currentOweVar* = (new FuzzyVariable "current_owe" 0 200000))

(defglobal ?*expVar* = (new FuzzyVariable "exp" 0.0 15.0))

(defglobal ?*business_owedVar* = (new FuzzyVariable "bus" 0 10))

(defglobal ?*eduVar* = (new FuzzyVariable "edu" 0 15))

(defglobal ?*amtInvestmentVar* = (new FuzzyVariable "inv" 0 500000))

(call nrc.fuzzy.FuzzyValue setMatchThreshold 0.1)

;---------------------------------------------
; RULE 0:  INITIALISING GLOBAL VARIABLES
;---------------------------------------------

(defrule MAIN::init-FuzzyVariables
    (declare (salience 101))
    ?Business_loan_applicant <- (Business_loan_applicant (applicant_name ?applicant_name))
    =>
    (call ?*currentOweVar* addTerm "low" (new ZFuzzySet 0 100000))
    (?*currentOweVar* addTerm "medium" (new TrapezoidFuzzySet 100001 140000 160000 180000))
    (?*currentOweVar* addTerm "High" (new SFuzzySet 180000 200000))
    (?*expVar* addTerm "low" (new ZFuzzySet 2.0 3.0))
    (?*expVar* addTerm "moderate" (new TrapezoidFuzzySet 4.0 6.0 7.0 8.0))
    (?*expVar* addTerm "experienced" (new SFuzzySet 10.0 12.0))
    (?*business_owedVar* addTerm "low" (new ZFuzzySet 1 2))
    (?*business_owedVar* addTerm "moderate" (new TrapezoidFuzzySet 3 4 5 6))
    (?*business_owedVar* addTerm "high" (new SFuzzySet 8 10))
    (?*eduVar* addTerm "low" (new ZFuzzySet 0 10))
    (?*eduVar* addTerm "moderate" (new TrapezoidFuzzySet 6 8 10 12))
    (?*eduVar* addTerm "educated" (new SFuzzySet 12 15))
    (?*amtInvestmentVar* addTerm "low" (new ZFuzzySet 0 40000))
    (?*amtInvestmentVar* addTerm "medium" (new TrapezoidFuzzySet 50000  70000 90000 100000))
    (?*amtInvestmentVar* addTerm "High" (new SFuzzySet 300000 500000))
    (assert (current_owe (new FuzzyValue ?*currentOweVar* (new SingletonFuzzySet ?Business_loan_applicant.current_owe))))
    (assert (experience (new FuzzyValue ?*expVar* (new SingletonFuzzySet ?Business_loan_applicant.exp))))
    (assert (business_owned (new FuzzyValue ?*business_owedVar* (new SingletonFuzzySet ?Business_loan_applicant.bus))))
	(assert (education (new FuzzyValue ?*eduVar* (new SingletonFuzzySet ?Business_loan_applicant.edu))))
    (assert (amount_invested (new FuzzyValue ?*amtInvestmentVar* (new SingletonFuzzySet ?Business_loan_applicant.inv))))        
    )

;---------------------------------------------
; RULE 1:  WELCOME TO THE EXPERT SYSTEM
;---------------------------------------------
(defrule LoanSanction
    (declare (salience 100))
    ?Business_loan_applicant <- (Business_loan_applicant (applicant_name ?applicant_name))
    =>
    (printout t crlf)
    (printout t "----------------------------------------------------------" crlf)
    (printout t "WELCOME TO THE EXPERT SYSTEM FOR BUSINESS LOAN APPLICATION" crlf)
    (printout t "Assert the initial values of an applicant and the decision will be made" crlf)
    (printout t "-----------------------------------------------------------------------" crlf)
    (printout t crlf)    
)
    
;-----------------------------------------------------------
; RULE 2:  PRINTING THE USER's INPUT(asserted by the grader)
;-----------------------------------------------------------    
 (defrule initial
    (declare (salience 99))
    ?Business_loan_applicant <- (Business_loan_applicant (applicant_name ?applicant_name))
    =>
    (printout t "BUSINESS loan application for "?Business_loan_applicant.applicant_name " under review." crlf)
    (printout t "Following is the Applicant's information that was feeded into our system" crlf)
    (printout t "Applicants legal age: " ?Business_loan_applicant.age " years" crlf)
    (printout t "Amount currently owed by applicant is: " ?Business_loan_applicant.current_owe " Dollars" crlf)
    (printout t "Any pending payment of previosuly sanctioned loans?(Yes/No): " ?Business_loan_applicant.paid crlf)
    (printout t "Applicant's Years of experience in related business: " ?Business_loan_applicant.exp crlf)
    (printout t "Total number of businesses owned by applicant: " ?Business_loan_applicant.bus crlf)
    (printout t "Years of education: " ?Business_loan_applicant.edu "years" crlf)
    (printout t "Total amount invested so far by the applicant in the business is: " ?Business_loan_applicant.inv " Dollars" crlf)
    (printout t "Does Applicant have the ability to repay the loan?(Good/Poor): " ?Business_loan_applicant.repay crlf)
    (printout t "Applicant's ability to manage the loan?(Good/Average/Poor): " ?Business_loan_applicant.mgmt crlf)
    (printout t "Credit Rating?(Good/Poor): " ?Business_loan_applicant.credit crlf)
  	)
    
;----------------------------------------------------------------------------------------
; RULE 3:  SUGGESTIONS PROVIDED BY THE SYSTEM TO THE BANK ON THE BASIS OF APPLICANTS INFO
;----------------------------------------------------------------------------------------- 
(defrule LoanSanctionSuggestions
    (declare (salience 98))
    ?Business_loan_applicant <- (Business_loan_applicant (applicant_name ?applicant_name))
    =>
    (printout t crlf)
    (printout t "--------------------------------------------------------" crlf)
    (printout t "Following are the suggestions & interpretations derived: " crlf)
    (printout t "---------------------------------------------------------" crlf)
    (printout t crlf) 
    )
;---------------------------------------------------------------------------------------
; RULE 4:  TO CHECK WHETHER APPLICANT'S AGE SATISFIES THE BANK'S LOAN SACNTION CRITERIA
;---------------------------------------------------------------------------------------

(defrule ageCheck
    (declare (salience 97))
    ?Business_loan_applicant <- (Business_loan_applicant (applicant_name ?applicant_name))
    =>
    (if (< ?Business_loan_applicant.age 18) then
        (printout t "Applicant is under the legal age set by the bank to apply for the loan.Bank can't process the loan application further." crlf)
        )
    )

;---------------------------------------------------------------------------------------------
; RULE 5:  TO CHECK WHETHER APPLCIANT'S CURRENTLY OWED AMOUNT SATISFIES THE BANKS POLICY
;--------------------------------------------------------------------------------------------

;Rule 5-i
(defrule currentOwed_Low
    (declare (salience 96))
    (current_owe ?Business_loan_applicant&:(fuzzy-match ?Business_loan_applicant "low"))
    =>
    (printout t "Applicant currently owes minimal amount to be owed as debt.Bank can't process the loan application further." crlf)  
    )

;Rule 5-ii
(defrule currentOwed_Medium
    (declare (salience 96))
    (current_owe ?Business_loan_applicant&:(fuzzy-match ?Business_loan_applicant "medium"))
    =>
    (printout t "Applicant owes debt in moderate range.While this is not the only criteria,there are equal chances of application being rejected or accepted. " crlf)  
    )

;Rule 5-iii
(defrule currentOwed_High
    (declare (salience 96))
    (current_owe ?Business_loan_applicant&:(fuzzy-match ?Business_loan_applicant "high"))
    =>
    (printout t "Applicant owes more than the permitted amount as debt. Highly likely for application to get rejected." crlf)  
    )
    
;----------------------------------------------------------------------------------------------
; RULE 6:  IF THE APPLICANT HAS PREVIOUSLY BORROWED LOANS UNPAID, BANK REJECTS THE LOAN REQUEST
;----------------------------------------------------------------------------------------------  
 
(defrule paidborrowedLoansCheck
    (declare (salience 95))
    ?Business_loan_applicant <- (Business_loan_applicant (applicant_name ?applicant_name))
    =>
    (if (= ?Business_loan_applicant.paid  No) then
        (printout t "Applicant haven't cleared the loan/s previously borrowed by him/her.Bank can't process the loan application further." crlf)
        )
    )
       
 
;------------------------------------------------------------------------------------
; RULE 7:  IF YEARS OF EXPERIENCE IS LESS THAN THE NEEDED YEARS,LOAN GETS REJETCED.
;------------------------------------------------------------------------------------  

;Rule 7-i
(defrule experienceCheck_Low
    (declare (salience 94))
    (experience ?Business_loan_applicant&:(fuzzy-match ?Business_loan_applicant "low"))
    =>
    (printout t "Applicant got a really low experience.While this may not necessarily be a red flag, he/she can still apply depending on the other factors." crlf)  
    )

;Rule 7-ii
(defrule experienceCheck_Moderate
    (declare (salience 94))
    (experience ?Business_loan_applicant&:(fuzzy-match ?Business_loan_applicant "moderate"))
    =>
    (printout t "Applicant has a decent experience in his/her line of business.This might be a positive indicator while evaluating the loan application" crlf)  
    )

;Rule 7-iii
(defrule experienceCheck_Experienced
    (declare (salience 94))
    (experience ?Business_loan_applicant&:(fuzzy-match ?Business_loan_applicant "experienced"))
    =>
    (printout t "Applicant has extensive experience in business. This boosts overall profile and makes his/her application stronger." crlf)  
    )
    
;-------------------------------------------------------------------------------------------------------
; RULE 8:  IF THE NUMBER OF BUSINESSES OWNED BY APPLICANT DOESNOT MATCH CRITERIA,CAN'T APPLY FOR THE LOAN
;------------------------------------------------------------------- -------------------------------------

;Rule 8-i
(defrule businessesOwnedCheck
    (declare (salience 93))
    (business_owned ?Business_loan_applicant&:(fuzzy-match ?Business_loan_applicant "low"))
    =>
        (printout t "Bank Needs atleast 3 businesses as a proof for an asset inorder to make the application strong." crlf)
        )
    

;Rule 8-ii
(defrule businessesOwnedCheck
    (declare (salience 93))
    (business_owned ?Business_loan_applicant&:(fuzzy-match ?Business_loan_applicant "moderate"))
    =>
        (printout t "Applicant have enough businesses as an asset to carry on with the application." crlf)
        )

;Rule 8-iii
(defrule businessesOwnedCheck
    (declare (salience 93))
    (business_owned ?Business_loan_applicant&:(fuzzy-match ?Business_loan_applicant "high"))
    =>
        (printout t "Applicant owns numerous businesses as an asset.This improves the credibility of the application." crlf)
        )
;---------------------------------------------------------------------------------------------------
; RULE 9:  MINIMUM YEARS OF EDUCATION REQUIRED,IF NOT,APPLICANT IS NOT ELIGIBLE TO APPLY FOR THE LOAN
;----------------------------------------------------------------------------------------------------- 
 
;Rule 9-i
(defrule educationCheck
    (declare (salience 92))
    (education ?Business_loan_applicant&:(fuzzy-match ?Business_loan_applicant "low"))
    =>
        (printout t "The required qualification in education is not satisifed.Vey Low." crlf)
        )

;Rule 9-ii
(defrule educationCheck
    (declare (salience 92))
    (education ?Business_loan_applicant&:(fuzzy-match ?Business_loan_applicant "moderate"))
    =>
        (printout t "Applicant have a moderate education qualification." crlf)
        )

;Rule 9-iii
(defrule educationCheck
    (declare (salience 92))
    (education ?Business_loan_applicant&:(fuzzy-match ?Business_loan_applicant "educated"))
    =>
        (printout t "Applicant has high education qualification.This strongly boosts the profile and improve his/her chances of application getting accepted. " crlf)
        )
;-----------------------------------------------------------------------------------------------------
; RULE 10:  IF INVESTMENT MADE BY APPLICANT IS NOT MORE THAN A CERTAIN AMOUNT,CANNOT APPLY FOR THE LOAN
;-----------------------------------------------------------------------------------------------------

;Rule 10-i
(defrule investmentCheck
    (declare (salience 91))
    (amount_invested ?Business_loan_applicant&:(fuzzy-match ?Business_loan_applicant "low"))
    =>
        (printout t "Applicant have NOT made ENOUGH investment in the business.Not eligible to apply for a loan till enough investment is made as an equity." crlf)
        )


;Rule 10-ii
(defrule investmentCheck
    (declare (salience 91))
    (amount_invested ?Business_loan_applicant&:(fuzzy-match ?Business_loan_applicant "moderate"))
    =>
        (printout t "Applicant have made moderate investment.No assurance guaranteed.Also check other suggestions." crlf)
        )

;Rule 10-iii
(defrule investmentCheck
    (declare (salience 91))
    (amount_invested ?Business_loan_applicant&:(fuzzy-match ?Business_loan_applicant "high"))
    =>
        (printout t "Applicant have made high investment in the business as compared to the other applicants and thus qualify for loan." crlf)
        )
;----------------------------------------------------------------------------------------------------------------------------------------------
; RULE 11:  IF APPLICANT HAS A POOR ABILITY TO REPAY THE LOAN,LOAN APLICATION REJECTED.
;-----------------------------------------------------------------------------------------------------------------------------------------------

(defrule repayAbilityCheck
    (declare (salience 90))
    ?Business_loan_applicant <- (Business_loan_applicant (applicant_name ?applicant_name))
    =>
    (if (= ?Business_loan_applicant.repay Poor) then
        (printout t "High risk of no returns since his/her ability to repay the loan back is poor" crlf)
        )
    )
    
;----------------------------------------------------------------------------------------------------------------------------------------
; RULE 12:  IF APPLICANT HAS A POOR MANAGEMENT ABILITY,LOAN APPLICATION REJECTED.
;----------------------------------------------------------------------------------------------------------------------------------------- 

(defrule managementAbilityCheck
    (declare (salience 89))
    ?Business_loan_applicant <- (Business_loan_applicant (applicant_name ?applicant_name))
    =>
    (if (= ?Business_loan_applicant.mgmt Poor) then
        (printout t "Applicant has poor management ability.Constant check required on his/her management status." crlf)
        )
    )
    
;----------------------------------------------------------------------------------------------------------------------------------------
; RULE 13:  IF CREDIT SCORE IS POOR,LOAN APPLICATION REJECTED.
;----------------------------------------------------------------------------------------------------------------------------------------- 

(defrule creditScoreCheck
    (declare (salience 88))
    ?Business_loan_applicant <- (Business_loan_applicant (applicant_name ?applicant_name))
    =>
    (if (= ?Business_loan_applicant.credit Poor) then
        (printout t "Applicant has a low credit score which is considered to be a negative flag. Bank can't process the loan application further." crlf)
        )
    )

;----------------------------------------------------------------------------------------------------------------------------------------
; RULE 14:  FINAL DECISION (ELIGIBLE TO APPLY FOR A LOAN OR NOT)
;----------------------------------------------------------------------------------------------------------------------------------------- 
(defrule finalDecision_Impparameters
    (declare (salience 87))
    ?x <- (Business_loan_applicant (applicant_name ?applicant_name))
    =>
 (if (or (< ?x.age 18) (= ?x.paid no) (> ?x.current_owe ?*upper_limit*) (= ?x.credit Poor)) then
        (printout t " " crlf)
		(printout t "--The Final Decision Suggested by the Expert System is that:--" crlf)
		(printout t " " crlf)
        (printout t "Applicant is not eligible to apply for the Business loan since one or all of the top 4 parameter/s(Legal age, Amount owed, previous loans paid and credit score) are not satisfied" crlf)
  else
        (printout t " " crlf)
		(printout t "--The Final Decision Suggested by the Expert System is that:--" crlf)
		(printout t " " crlf)
        (printout t "Applicant is eligible to apply for the Business loan since one or all of the top 4 parameter/s((Legal age, Amount owed, previous loans paid and credit score) ) are satisfied" crlf)
        )
    )

;----------------------------------------------------------------------------------------------------------------------------------------
; RULE 15:  PRINT FACTS
;----------------------------------------------------------------------------------------------------------------------------------------- 

(defrule printFacts
    (declare (salience 2))
    =>
    (printout t " " crlf)
	(printout t " " crlf)
    (facts)
    )


(defrule printFacts
    (declare (salience 1))
    =>
    (facts)
    )
(reset)
 
      
;---------------------------------------------
; TO GET THE USER's INPUT
;---------------------------------------------

(assert (Business_loan_applicant (applicant_name "Jon")
            (age 18) (current_owe 200000) (paid Yes)
            (exp 12.0)  (bus 10)  (edu 2)  (inv 90000)
            (repay Good)  (mgmt Poor)  (credit Poor)))
(run)                  