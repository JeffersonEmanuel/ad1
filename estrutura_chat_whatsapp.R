library(dplyr)
library(stringr)
library(readr)

# Recebe script por linha de comando
args = commandArgs(trailingOnly=TRUE)

# Le o arquivo de entrada e retira acentuacao e emojis das mensagens.
group_chat <- readLines(args[1], encoding = "UTF-8") %>% 
  iconv(from="UTF-8", to="ASCII//TRANSLIT") %>% 
  paste(sep=" ", collapse=" ")

# Secao de regex
day_regex <- "([0]?[:digit:]|[12][:digit:]|3[01])"
month_regex <- "([0]?[:digit:]|1[012])"
year_regex <- "(1[0-7])"
date_dmy_regex <- paste("(",day_regex,"/",month_regex,"/",year_regex,")",sep="")
date_mdy_regex <- paste("(",month_regex,"/",day_regex,"/",year_regex,")",sep="")
date_regex <- paste("(",date_dmy_regex,"|",date_mdy_regex,")",sep = "")

comma_regex <- "([, ]{2})"
time_regex <- "(([1]?[:digit:]:[0-5][:digit:] (AM|PM))|(([01][:digit:]|2[0-3])[:][0-5][:digit:]))"
dash_regex <- "([- ]{3})"
person_regex <- "((([[:alnum:]]+[[:blank:]]?){1,4})|([?]?[+][[:digit:]]+[[:blank:]][(]?[[:digit:]]+[)]?[[:blank:]][[:digit:]]+[-][[:digit:]]+)[?]?)"

base_line_regex <- paste(date_regex, comma_regex, time_regex, dash_regex, person_regex, sep="")
message_regex <- paste(base_line_regex, "[:]{1}", sep="")
action_regex <- paste(base_line_regex, "[:print:]+")

# Recupera informacoes da mensagem: Dia, hora e contato
informacoes_mensagem_lista <- group_chat %>% str_extract_all(message_regex)
informacoes_mensagem_modificada <- informacoes_mensagem_lista %>% 
  unlist() %>% 
  str_replace_all("[?]{1}:","") %>%
  str_replace(" - [?]?",", ")

informacoes_mensagem <- informacoes_mensagem_modificada %>%
  str_split(", ") %>% do.call(what=rbind) %>% tbl_df() %>% 
  rename(data = V1, hora = V2, id_pessoa = V3)

# Recupera o conteudo das mensagens
texto_original <- str_split(group_chat, message_regex) %>% 
  unlist() %>% 
  str_trim()

conteudo_texto <- texto_original[-1] %>% str_replace(action_regex, "")

# Checa se ha mencao a algum integrante do grupo
menciona_integrante <- conteudo_texto %>% str_detect(paste("@", person_regex, sep=""))

# Escreve as mensagens num csv de saida
historico_chat <- data.frame(informacoes_mensagem, conteudo_texto, menciona_integrante)
write_csv(historico_chat, "historico_grupo.csv")