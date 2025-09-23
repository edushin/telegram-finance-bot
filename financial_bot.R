# Complete Free Telegram Financial News Bot
# This version requires ZERO paid APIs and runs completely free

# Install required packages
if (!require("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(telegram.bot, xml2, httr, stringr, dplyr, lubridate)

# Load environment variables
if (file.exists(".env")) {
  readRenviron(".env")
} else if (Sys.getenv("GITHUB_ACTIONS") == "true") {
  # GitHub Actions will provide these as environment variables
  cat("Running in GitHub Actions environment\n")
} else {
  stop("Please create .env file with your bot credentials")
}

# Initialize bot
bot <- Bot(token = Sys.getenv("TELEGRAM_BOT_TOKEN"))

# Function to get financial news from Yahoo Finance RSS
get_yahoo_finance_news <- function() {
  tryCatch({
    # Yahoo Finance RSS feed - completely free
    rss_url <- "https://feeds.finance.yahoo.com/rss/2.0/headline"
    
    # Read RSS feed
    rss_content <- read_xml(rss_url)
    
    # Extract news items
    items <- xml_find_all(rss_content, "//item")
    
    if (length(items) > 0) {
      # Get top 6 news items
      headlines <- xml_text(xml_find_all(items[1:min(6, length(items))], "title"))
      links <- xml_text(xml_find_all(items[1:min(6, length(items))], "link"))
      descriptions <- xml_text(xml_find_all(items[1:min(6, length(items))], "description"))
      
      # Clean up descriptions (remove HTML tags)
      descriptions <- gsub("<[^>]*>", "", descriptions)
      descriptions <- gsub("&nbsp;", " ", descriptions)
      descriptions <- str_trim(descriptions)
      
      news_df <- data.frame(
        headline = headlines,
        link = links,
        description = descriptions,
        stringsAsFactors = FALSE
      )
      
      return(news_df)
    } else {
      return(get_backup_news())
    }
    
  }, error = function(e) {
    cat("Yahoo RSS failed:", e$message, "\n")
    return(get_backup_news())
  })
}

# Backup news function
get_backup_news <- function() {
  data.frame(
    headline = paste("Financial Markets Update -", format(Sys.Date(), "%B %d, %Y")),
    link = "https://finance.yahoo.com",
    description = "Check Yahoo Finance for the latest market updates and financial news.",
    stringsAsFactors = FALSE
  )
}

# Smart text analysis function (completely free)
analyze_financial_news <- function(headlines) {
  # Convert to lowercase for analysis
  text <- tolower(paste(headlines, collapse = " "))
  
  # Define market categories with keywords
  categories <- list(
    "📈 Stock Market" = c("stock", "shares", "market", "trading", "wall street", "nasdaq", "s&p", "dow"),
    "💰 Earnings & Revenue" = c("earnings", "profit", "revenue", "quarterly", "guidance", "beat", "miss"),
    "🏦 Federal Reserve & Policy" = c("fed", "federal reserve", "interest", "rate", "powell", "fomc", "monetary"),
    "📊 Economic Data" = c("inflation", "gdp", "employment", "jobs", "unemployment", "cpi", "economic"),
    "🌍 Global Markets" = c("china", "europe", "international", "global", "trade", "tariff", "brexit"),
    "₿ Cryptocurrency" = c("bitcoin", "crypto", "blockchain", "ethereum", "digital currency"),
    "🏢 Corporate News" = c("merger", "acquisition", "ceo", "partnership", "deal", "investment"),
    "⚡ Energy & Commodities" = c("oil", "gas", "energy", "gold", "commodity", "crude")
  )
  
  # Analyze categories
  category_matches <- sapply(categories, function(keywords) {
    sum(sapply(keywords, function(keyword) grepl(keyword, text)))
  })
  
  # Get top categories
  top_categories <- sort(category_matches[category_matches > 0], decreasing = TRUE)
  
  # Create smart summary
  if (length(top_categories) > 0) {
    summary_lines <- c()
    
    for (i in 1:min(3, length(top_categories))) {
      category_name <- names(top_categories)[i]
      count <- top_categories[i]
      summary_lines <- c(summary_lines, paste0("• ", category_name, " (", count, " mentions)"))
    }
    
    summary <- paste0(
      "🤖 *AI Analysis Summary:*\n",
      paste(summary_lines, collapse = "\n"),
      "\n\n📈 *Market Focus:* ",
      ifelse(grepl("market|stock|trading", text), "Active trading discussions", 
      ifelse(grepl("earnings|profit", text), "Earnings season focus",
      ifelse(grepl("fed|interest", text), "Federal Reserve policy focus", 
             "Mixed financial developments")))
    )
  } else {
    summary <- "🤖 *AI Analysis:* General financial market news and updates"
  }
  
  return(summary)
}

# Function to create and send the daily summary
send_daily_financial_summary <- function() {
  cat("🚀 Starting daily financial news summary...\n")
  
  # Get news
  news <- get_yahoo_finance_news()
  cat("📰 Retrieved", nrow(news), "news items\n")
  
  # Analyze news
  ai_summary <- analyze_financial_news(news$headline)
  cat("🤖 Generated AI analysis\n")
  
  # Create message
  message_text <- paste0(
    "📈 *Daily Financial Summary*\n",
    "📅 ", format(Sys.time(), "%A, %B %d, %Y at %I:%M %p"), "\n\n",
    ai_summary, "\n\n",
    "📰 *Top Headlines:*\n"
  )
  
  # Add headlines with links
  for (i in 1:min(nrow(news), 5)) {
    # Clean headline
    headline <- str_trim(news$headline[i])
    if (nchar(headline) > 80) {
      headline <- paste0(substr(headline, 1, 77), "...")
    }
    
    if (news$link[i] != "" && !is.na(news$link[i])) {
      message_text <- paste0(message_text, "• [", headline, "](", news$link[i], ")\n")
    } else {
      message_text <- paste0(message_text, "• ", headline, "\n")
    }
  }
  
  # Add footer
  message_text <- paste0(
    message_text,
    "\n🤖 *Automated by Free R Telegram Bot*\n",
    "_Powered by Yahoo Finance RSS & GitHub Actions_ ⚡"
  )
  
  # Send message
  tryCatch({
    response <- bot$sendMessage(
      chat_id = Sys.getenv("TELEGRAM_CHAT_ID"),
      text = message_text,
      parse_mode = "Markdown",
      disable_web_page_preview = TRUE
    )
    
    if (response$ok) {
      cat("✅ Message sent successfully!\n")
      cat("📱 Sent to chat ID:", Sys.getenv("TELEGRAM_CHAT_ID"), "\n")
    } else {
      cat("❌ Failed to send message:", response$description, "\n")
    }
    
  }, error = function(e) {
    cat("❌ Error sending message:", e$message, "\n")
    
    # Try sending a simple fallback message
    tryCatch({
      fallback_msg <- paste0(
        "📈 Daily Financial Summary - ", format(Sys.Date(), "%B %d, %Y"),
        "\n\n❗ Technical issue occurred. Please check Yahoo Finance manually: https://finance.yahoo.com"
      )
      
      bot$sendMessage(
        chat_id = Sys.getenv("TELEGRAM_CHAT_ID"),
        text = fallback_msg
      )
      cat("📱 Sent fallback message\n")
    }, error = function(e2) {
      cat("❌ Fallback message also failed:", e2$message, "\n")
    })
  })
}

# Test function
test_bot_now <- function() {
  cat("🧪 Testing bot immediately...\n")
  send_daily_financial_summary()
}

# Main execution
if (!interactive()) {
  # This runs when script is called from command line (GitHub Actions)
  send_daily_financial_summary()
} else {
  # This runs in RStudio for testing
  cat("🎯 Bot loaded! Use test_bot_now() to test immediately\n")
  cat("📋 Available functions:\n")
  cat("   • test_bot_now() - Test the bot right now\n")
  cat("   • send_daily_financial_summary() - Send daily summary\n")
}

