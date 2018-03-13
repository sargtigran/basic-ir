
#include "ast.hxx"
#include "lexeme.hxx"
#include "scanner.hxx"

#include <exception>
#include <string>
#include <memory>

namespace basic {
//! @brief Շարահյուսական վերլուծիչը
class Parser {
private:
    //! @brief Վերլուծված ծրագրի ցուցիչը, միաժամանակ նաև
    //! վերլուծության ծառի արմատը
    ProgramPtr module;

    Scanner scanner;  //!< բառային վերլուծիչը
    Lexeme lookahead; //!< հերթական լեքսեմը

    //! @brief անորոշ հղումներ. բանալին ենթածրագրի անունն է,
    //! իսկ արժեքը դրան հղվող Apply օբյեկտների ցուցակը
    std::map<std::string,std::list<ApplyPtr>> unresolved;

public:
    //! @brief Շարահյուսական վերլուծիչի կոնստրուկտորը
    //!
    //! @param filename - վերլուծվելիք ֆայլի ճանապարհը
    Parser( const std::string& filename );

    //! @brief Շարահյուսական վերլուծիչի դեստրուկտորը
    ~Parser();

    //! @brief Վերլուծում է ամբողջ ծրագիրը
    ProgramPtr parse();

private:
    //! @brief Վերլուծում է BASIC-IR ծրագիրը
    void parseProgram();

    //! @brief Վերլուծում է ենթածրագրի սահմանումը
    void parseSubroutine();

    //! @brief Վերլուծում է հրամանների հաջորդականությունը
    StatementPtr parseStatements();

    //! @brief Վերլուծում է ներմուծման @c INPUT հրամանը
    StatementPtr parseInput();

    //! @brief Վերլուծում է արտածման @c PRINT հրամանը
    StatementPtr parsePrint();

    //! @brief Վերլուծում է վերագրման @c LET հրամանը
    StatementPtr parseLet();

    //! @brief Վերլուծում է ճյուղավորման @c IF հրամանը
    StatementPtr parseIf();

    //! @brief Վերլուծում է պայմանով ցիկլի @c WHILE հրամանը
    StatementPtr parseWhile();

    //! @brief Վերլուծում է պարամետրով ցիկլի @c FOR հրամանը
    StatementPtr parseFor();

    //! @brief Վերլուծում է պրոցեդուրայի կանչի @c CALL հրամանը
    StatementPtr parseCall();

    //! @brief Վերլուծում է համեմատման գործողությունները
    //!
    //! Վերլուծում է հավասար է, հավասար չէ, մեծ է, մեծ է կամ
    //! հավասար, փոքր է, փոքր է կամ հավասար գործողությունները։
    ExpressionPtr parseExpression();

    //! @brief Վերլուծում է ադիտիվ գործողությունները
    //!
    //! Վերլուծում է թվերի գումարման, հանման, տողերի կցման
    //! և դիզյունկցիայի գործողությունները։
    ExpressionPtr parseAddition();

    //! @brief Վերլուծում է մուլտիպլիկատիվ գործողությունները
    //!
    //! Վերլուծում է բազմապատկման, բաժանման մնացորդի որոշման
    //! և կոնյունկցիայի գործողությունները։
    ExpressionPtr parseMultiplication();

    //! @brief Վերլուծում է աստիճան բարձրացնելու գործողությունը
    ExpressionPtr parsePower();

    //! @brief Վերլուծում է պարզագույն արտահայտությունները
    //!
    //! Վերլուծում է թվային ու տեքստային հաստատունները, փոփոխականները,
    //! ֆունկցիայի կիրառումը և խմբավորման փակագծերի մեջ վերցրած
    //! արտահայտությունները։
    ExpressionPtr parseFactor();

    //! @brief Վերլուծում է նոր տողերի անցման նիշերը
    void parseNewLines();

    void match( Token tok );

    //! @brief Հայտարարում է BASIC-IR լեզվի ներդրված ենթածրագիր
    //!
    //! @param nm - ենթածրագրի անունը
    //! @param ps - պարամետերեի ցուցակը
    //! @param rv - ֆունկցիա է կամ պրոցեդուրա
    void declareBuiltIn( const std::string& nm, const std::vector<std::string>& ps, bool rv );

    //! @brief Ստեղծում է լոկալ փոփոխական կամ վերադարձնում է արդեն գոյություն
    //! ունեցող փոփոխականի հասցեն
    //!
    //! Եթե լոկալ տիրույթում արդեն սահմանված է @c nm անունով փոփոխական,
    //! ապա վերադարձնում է դրա ցուցիչը։
    //! Եթե դեռ սահմանված չէ, ապա ստեղծել նոր @c Variable օբյեկտ, և 
    //! վերադարձնել դրա ցուցիչը։
    //! Եթե հարցումն արվում է rval-ի համար, ապա նոր օբյեկտ չստեղծել։
    //! Եթե հարցումը կատարվում է rval-ի համար, ապա ընթացիկ ենթածրագրի
    //! անունը փոփոխական չհամարել։
    //!
    //! @param nm   - փոփոխականի անունը
    //! @param rval - @c true է, եթե փոփոխականը վերագրման աջ կողմում է
    VariablePtr getVariable( const std::string& nm, bool rval );

    //! @brief Ենթածրագրի կանչի համար գտնում ու վերադարձնում է ենթածրագիր
    //! օբյեկտի հասցեն, ինչպես նաև ստուգում է արգումենտների ու պարամետրերի
    //! տիպերի համապատասխանությունը։
    //!
    //! @param nm   - ենթածրագրի անունը
    //! @param ags  - կանչի արգումենտները
    //! @param func - @c true է, եթե ենթածրագիրը կիրառված է որպես ֆունկցիա
    SubroutinePtr getSubroutine( const std::string& nm,
        const std::vector<ExpressionPtr>& ags, bool func );
};

bool equalNames( const std::string& no, const std::string& ni );
} // basic
