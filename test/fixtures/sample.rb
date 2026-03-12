module Auth
  class Authenticator
    def initialize(secret)
      @secret = secret
    end

    def verify(token)
      token == @secret
    end

    private

    def hash_token(token)
      Digest::SHA256.hexdigest(token)
    end
  end
end

def standalone_func
  puts "hello"
end
