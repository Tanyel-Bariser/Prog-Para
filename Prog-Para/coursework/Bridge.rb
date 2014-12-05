# The program is a modified version of the card game Bridge.
# Author:: Tanyel Bariser
# Course:: MSc Computer Science
# Deadline:: Sunday, 12th January, 23:55

# Numeric is an open class and therefore allows the insertion
# of a new method, in this case card.
class Numeric
	# Returns a string relevant to a card number.
	#
	# * Ace is always 1, so here King beats Ace
	def card
		return "King" if self == 13
		return "Queen" if self == 12
		return "Jack" if self == 11
		return "Ace" if self == 1
		self
	end
end

# Represents a card with a suit and a rank, which can be found
# out but they are immutable.
class Card
	attr_reader :suit, :rank
	SUIT = ["Hearts", "Diamonds", "Spades", "Clubs"]
	RANK = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]
	# Initialize method allows the creation of card objects with
	# distinct suits and ranks easily through use of modulo operator.
	def initialize(card_num)
		@suit = SUIT[card_num % SUIT.size]
		@rank = RANK[card_num % RANK.size]
	end
end

# Represents a player who can shuffle, deal and play cards.
class Player
	attr_reader :name, :hand
	# All players have a name and a hand.
	def initialize(name)
		@name = name
		@hand = []
	end
	# The player deals 13 cards to every player including themselves.
	def deal(deck, players)
		puts "#{@name} will shuffle and deal the cards.\n\n\n"
		deck.shuffle!
		13.times do
			players.each do |player|
				player.take(deck.pop)
			end
		end
	end
	# The player prints the card taken and adds to their hand.
	def take(card)
		@hand.push(card)
		puts "#{@name} has taken #{card.rank.card} of #{card.suit}"
	end
	# The player plays a hand of the same suit as the lead if he/she
	# has one, otherwise pops any card from their hand.
	def play_card(suit)
		@hand.each do |card|
			if (card.suit == suit) then
				return hand.delete(card)
			end
		end
		@hand.pop
	end
end

# Mixin for class Bridge for initialising the deck and players
#
# * both methods are private
module Create
	# Initialises 52 distinct cards and calls freeze for immutability
	def create_deck
		@deck = (1..52).map do |card_num|
			Card.new(card_num).freeze
		end
	end
	# Initialises four players named North, East, South and West
	def create_players
		@players = [Player.new("North"), Player.new("East"),
					Player.new("South"), Player.new("West")]
	end
	
	private :create_deck, :create_players
end

# class Bridge controls the whole game
class Bridge
	include Create
	attr_reader :deck, :players
	# Includes the methods in the Create module to initialise the deck and players
	def initialize
		create_deck
		create_players
	end
	# The main method for controlling the Bridge game
	#
	# * the lead player plays a card and the other players
	# * try to play a card of the same suit.
	# * the player who has the highest rank of the same
	# * suit wins the trick and leads the next trick.
	# * scores are not recorded and therefore the game has no winner.
	def play_game
		#Randomly chooses player to deal cards and then a player to lead
		@players[rand(@players.size)].deal(@deck, @players)
		lead = @players[rand(@players.size)]
		#Start of main game
		for trick in (1..13) do
			puts "\n\nTrick #{trick}: #{lead.name} leads...\n\n"
			#Lead player plays first card
			first_card = lead.hand.pop
			lead_suit = first_card.suit
			puts "#{lead.name} has led with #{first_card.rank.card} of #{lead_suit}"
			cards_played = {lead => first_card}
			#Other players play their cards
			i = @players.index(lead)
			3.times do
				i = i + 1
				next_player = @players[i % 4]
				card = next_player.play_card(lead_suit)
				cards_played[next_player] = card
				puts "#{next_player.name} has played #{card.rank.card} of #{card.suit}"
			end
			puts "\n\n"
			#Works out which player won the trick and assigns winner to lead next trick
			max_rank = first_card.rank
			best_card = first_card
			cards_played.values.each do |card|
				if (card.suit == lead_suit) then
					if (card.rank > max_rank) then
						best_card = card
						max_rank = card.rank
					end
				end
			end
			winner = cards_played.key(best_card)
			puts "#{winner.name} won the trick with the #{best_card.rank.card} of #{best_card.suit}"
			lead = winner
		end	
	end
end


Bridge.new.play_game